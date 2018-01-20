/* This file implements a trinarized convolution summation
 */
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object TriConvSum {

  def mapToWires[ T <: Bits ](
    conv : Seq[Seq[Seq[Int]]],
    currData : Seq[Seq[Seq[T]]]
  ) : (Seq[T], Seq[T]) = {
    val posNums = ArrayBuffer[T]()
    val negNums = ArrayBuffer[T]()

    for ( lyr1 <- conv.zip( currData ) ) {
      for ( lyr2 <- lyr1._1.zip( lyr1._2 ) ) {
        for ( lyr3 <- lyr2._1.zip( lyr2._2 ) ) {
          if ( lyr3._1 == 1 )
            posNums += lyr3._2
          if ( lyr3._1 == -1 )
            negNums += lyr3._2
        }
      }
    }
    ( posNums.toSeq, negNums.toSeq )
  }

}

private class ParrallelTriConvSum (
  dtype : SInt,
  weights : Seq[Seq[Seq[Seq[Int]]]]
) extends Module {

  val io = IO( new Bundle {
    val dataIn = Input(Vec( weights(0).size, Vec( weights(0)(0).size, Vec( weights(0)(0)(0).size, dtype.cloneType ))))
    val dataOut = Output(Vec( weights.size, dtype.cloneType ))
  })

  def computeSum( posNums : Seq[SInt], negNums : Seq[SInt] ) : (SInt, Int, Int) = {
    var plusList = posNums.toList
    var minusList = negNums.toList

    val zero = 0.U.asTypeOf( dtype )

    var opsTotal = 0
    var stages = 0
    while ( plusList.size > 1 || minusList.size > 0 ) {
      // group by 3, partition on if not single op as should just add otherwise
      val plusOps : (List[List[SInt]], List[List[SInt]]) = plusList.grouped(3).toList.partition( _.size > 1 )
      plusList = plusOps._1.map( x => RegNext( x.reduce( ( a, b ) => a + b ) ) ).toList
      opsTotal += plusOps._1.size
      val negOps = minusList.grouped(3).toList.partition( _.size > 1 )
      opsTotal += negOps._1.size
      minusList = negOps._1.map( x => RegNext( x.reduce( _ + _ ) ) ).toList

      // may have 1 left over from neither, only one, or both
      if ( plusOps._2.size == 1 && negOps._2.size == 1 ) {
        // both
        plusList = RegNext( plusOps._2.head.head - negOps._2.head.head ) :: plusList
        opsTotal += 1
      } else if ( plusOps._2.size == 1 ) {
        // just plus
        plusList = RegNext( plusOps._2.head.head ) :: plusList
        opsTotal += 1
      } else if ( negOps._2.size == 1 ) {
        // just minus
        // can either take the neg and add to plus or keep in minus, check sizes to see
        if ( minusList.size % 3 == 0 )
          plusList = RegNext( zero - negOps._2.head.head ) :: plusList
        else
          minusList = RegNext( negOps._2.head.head ) :: minusList
        opsTotal += 1
      } // else neither so ignore

      stages += 1
    }
    if ( plusList.size == 0 )
      return ( zero, 0, 0 )
    return ( plusList.head, stages, opsTotal )
  }

  val currData = io.dataIn
  val outSums = weights.map( conv => {
    val numsOut = TriConvSum.mapToWires( conv, io.dataIn )
    val res = computeSum( numsOut._1, numsOut._2 )
    ( res._1, res._2 )
  })

  val latency = outSums.map( _._2 ).max

  io.dataOut := Vec( outSums.map( r => ShiftRegister( r._1, latency - r._2 )) )

}

private class SerialTriConvSum (
  dtype : SInt,
  weights : Seq[Seq[Seq[Seq[Int]]]],
  bitWidth : Int
) extends Module {

  val io = IO( new Bundle {
    val start = Input( Bool() )
    val dataIn = Input( Vec( weights(0).size, Vec( weights(0)(0).size, Vec( weights(0)(0)(0).size, dtype.cloneType ))))
    val dataOut = Output(Vec( weights.size, dtype.cloneType ))
  })

  val inWidth = dtype.cloneType.getWidth
  val nIter = inWidth / bitWidth
  val log2BW = log2Ceil( bitWidth )
  val log2Iter = log2Ceil( nIter )

  def computeSum( posNums : Seq[UInt], negNums : Seq[UInt], startReg : Bool ) : (UInt, Bool, Int) = {
    var plusList = posNums.toList.map( x => { ( x, startReg ) } )
    var minusList = negNums.toList.map( x => { ( x, startReg ) } )

    var stages = 0
    while ( plusList.size > 1 || minusList.size > 0 ) {
      val plusOps = plusList.grouped( 2 ).toList.partition( _.size > 1 )
      plusList = plusOps._1.map( x => SerialAdder.add( x(0)._1, x(1)._1, x(0)._2, bitWidth ) )
      val minusOps = minusList.grouped( 2 ).toList.partition( _.size > 1 )
      minusList = minusOps._1.map( x => SerialAdder.add( x(0)._1, x(1)._1, x(0)._2, bitWidth ) )
      if ( plusOps._2.size == 1 && minusOps._2.size == 1 ) {
        plusList = SerialAdder.sub(
          plusOps._2.head.head._1,
          minusOps._2.head.head._1,
          plusOps._2.head.head._2,
          bitWidth
        ) :: plusList
      } else if ( plusOps._2.size == 1 )
        plusList = (
          RegNext( plusOps._2.head.head._1 ),
          RegNext( plusOps._2.head.head._2 ) ) :: plusList
      else if ( minusOps._2.size == 1 ) {
        plusList = SerialAdder.sub(
          0.U( bitWidth.W ),
          minusOps._2.head.head._1,
          minusOps._2.head.head._2,
          bitWidth
        ) :: plusList
      }
      stages += 1
    }
    if ( plusList.size == 0 )
      return ( 0.U( bitWidth.W ), true.B, 0 )
    return ( plusList.head._1, plusList.head._2, stages )
  }

  val nibbleCntr = RegInit( 0.U( log2Iter.W ) )
  when ( nibbleCntr > 0.U || io.start ) {
    nibbleCntr := nibbleCntr + 1.U
  }
  val nibReg = RegNext( nibbleCntr )
  val dataNibble = Wire( Vec(
    weights(0).size,
    Vec( weights(0)(0).size,
      Vec( weights(0)(0)(0).size,
        0.U( bitWidth.W ).cloneType ))))
  for ( x <- 0 until nIter ) {
    for ( i <- 0 until weights(0).size ) {
      for ( j <- 0 until weights(0)(0).size ) {
        for ( k <- 0 until weights(0)(0)(0).size ) {
          val thisNibble = RegNext( io.dataIn( i )( j )( k )( bitWidth * ( x + 1 ) - 1, bitWidth*x ) )
          if ( x > 0 ) {
            when ( nibReg === x.U ) {
              dataNibble( i )( j )( k ) := thisNibble
            }
          } else
              dataNibble( i )( j )( k ) := thisNibble
        }
      }
    }
  }

  def pipelineFanout( nbl : List[Vec[Vec[Vec[UInt]]]], noReg : Int, noOut : Int ) : List[Vec[Vec[Vec[UInt]]]] = {
    val noInLyr = ( 1 to noReg ).map( n => {
      math.round( math.exp( math.log( noOut ) * n / noReg ) ).toInt
    }).toList
    val nblLyrs = ArrayBuffer[List[Vec[Vec[Vec[UInt]]]]]()
    nblLyrs += nbl
    for ( n <- noInLyr ) {
      val lastLyr = nblLyrs.last
      val fanout = ( n / lastLyr.size ).toInt
      val fanouts = ( 0 until lastLyr.size ).map( i => {
        if ( i >= n % lastLyr.size )
          fanout
        else
          fanout + 1
      }).toList
      val newLyr = fanouts.zipWithIndex.map( f => {
        List.fill( f._1 ) { RegNext( lastLyr( f._2 ) ) }
      }).toList.reduce( _ ++ _ )
      nblLyrs += newLyr
    }
    nblLyrs.last
  }

  val fanoutReg = 2
  val startReg = ShiftRegister( io.start, fanoutReg + 1 )

  /*
  val dataFanout = pipelineFanout( List(dataNibble), fanoutReg, weights.size )
  val outSums = weights.zip( dataFanout ).map( conv => {
    val numsOut = TriConvSum.mapToWires( conv._1, conv._2 )
    computeSum( numsOut._1, numsOut._2, startReg )
  })
   */
  val dataFanout = ShiftRegister( dataNibble, fanoutReg )
  val outSums = weights.map( conv => {
    val numsOut = TriConvSum.mapToWires( conv, dataFanout )
    computeSum( numsOut._1, numsOut._2, startReg )
  })

  val nibbleLat = outSums.map( _._3 ).max

  val nibbleOut = outSums.map( r => ShiftRegister( r._1, nibbleLat - r._3 ))
  val nibbleStarts = outSums.map( r => ShiftRegister( r._2, nibbleLat - r._3 ))

  val unnibble = nibbleOut.zip( nibbleStarts ).map( x => {
    val nibCntr = RegInit( 0.U( log2Iter.W ) )
    when ( x._2 || nibCntr > 0.U ) {
      nibCntr := nibCntr + 1.U
    }
    val outReg = Reg( Vec( nIter, 0.U( bitWidth.W ).cloneType ) )
    for ( i <- 0 until nIter ) {
      when ( nibCntr === i.U ) {
        outReg( i ) := x._1
      }
    }
    outReg.reverse.reduce( _ ## _ ).asTypeOf( dtype )
  })

  val latency = nibbleLat + 2 + fanoutReg
  io.dataOut := Vec( unnibble )
}

/* take in 1, 0, -1 weights
 * perform the convolution on them
 */
class TriConvSum (
  val dtype : SInt,
  val weights : Seq[Seq[Seq[Seq[Int]]]],
  tput : Double
) extends NNLayer(
  dtype,
  math.ceil( tput ),
  weights(0)(0)(0).size * weights(0)(0).size * weights(0).size,
  weights.size,
  math.ceil( tput ).toInt
) {

  io.dataIn.ready := true.B // always ready with tput = 1
  for ( d <- io.vldMask )
    d := false.B

  val tPutRounded = math.ceil( throughput ).toInt

  val inWidth = dtype.cloneType.getWidth
  val bitWidth = math.ceil( inWidth * tput ).toInt
  val log2Iter = math.max( log2Ceil( math.ceil( inWidth.toFloat / bitWidth ).toInt ), 1 ).toInt
  val rdyCnt = RegInit( 0.U( log2Iter.W ) ) // NOTE: this is broken for not powers of 2
  if ( bitWidth < inWidth ) {
    // set ready signals
    when ( io.dataIn.valid || rdyCnt > 0.U ) {
      rdyCnt := rdyCnt + 1.U
    }
    io.dataIn.ready := ( rdyCnt === (( 1 << log2Iter) - 1).U || ( !io.dataIn.valid && rdyCnt === 0.U ) )
  }

  val noDelayReg = 1
  val startReg = ShiftRegister( io.dataIn.valid && rdyCnt === 0.U, noDelayReg )

  val dataVec = ShiftRegister( inIOToVVV( weights(0)(0).size, weights(0)(0)(0).size ), noDelayReg )
  val convRes = ( 0 until tPutRounded ).map( idx => {
    if ( bitWidth < inWidth ) {
      val pConv = Module( new SerialTriConvSum( dtype, weights, bitWidth ) )
      pConv.io.start := startReg
      pConv.io.dataIn := Vec( ( 0 until weights(0).size ).map( wIdx => {
        dataVec( wIdx + weights(0).size * idx )
      }))
      ( pConv.io.dataOut, pConv.latency )
    } else {
      val pConv = Module( new ParrallelTriConvSum( dtype, weights ) )
      pConv.io.dataIn := Vec( ( 0 until weights(0).size ).map( wIdx => {
        dataVec( wIdx + weights(0).size * idx )
      }))
      ( pConv.io.dataOut, pConv.latency )
    }
  })

  val latency = convRes.map( _._2 ).max + noDelayReg

  val convOut = convRes.map( o => {
    ShiftRegister( o._1, latency - noDelayReg - o._2 )
  }).reduce( (a, b) => Vec( a ++ b ) )

  val convValid = ShiftRegister( io.dataIn.valid && io.dataIn.ready, latency, false.B, true.B )

  io.dataOut.bits := convOut
  io.dataOut.valid := convValid
}
