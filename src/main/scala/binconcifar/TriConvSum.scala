/* This file implements a trinarized convolution summation
 */
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

private class ParrallelTriConvSum[T <: Bits with Num[T]]( dtype : T, weights : Seq[Seq[Seq[Seq[Int]]]] ) extends Module {

  val io = IO( new Bundle {
    val dataIn = Input(Vec( weights(0).size, Vec( weights(0)(0).size, Vec( weights(0)(0)(0).size, dtype.cloneType ))))
    val dataOut = Output(Vec( weights.size, dtype.cloneType ))
  })

  def mapToWires( conv : Seq[Seq[Seq[Int]]], currData : Seq[Seq[Seq[T]]] ) : (Seq[T], Seq[T]) = {
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

  def computeSum( posNums : Seq[T], negNums : Seq[T] ) : (T, Int, Int) = {
    var plusList = posNums.toList
    var minusList = negNums.toList

    val zero = 0.U.asTypeOf( dtype )

    var opsTotal = 0
    var stages = 0
    while ( plusList.size > 1 || minusList.size > 0 ) {
      // group by 3, partition on if not single op as should just add otherwise
      val plusOps : (List[List[T]], List[List[T]]) = plusList.grouped(3).toList.partition( _.size > 1 )
      plusList = plusOps._1.map( x => RegNext( x.reduce( _ + _ ) ) ).toList
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
    val numsOut = mapToWires( conv, io.dataIn )
    val res = computeSum( numsOut._1, numsOut._2 )
    ( res._1, res._2 )
  })

  val latency = outSums.map( _._2 ).max

  io.dataOut := Vec( outSums.map( r => ShiftRegister( r._1, latency - r._2 )) )


}

/* take in 1, 0, -1 weights
 * perform the convolution on them
 */
class TriConvSum( val weights : Seq[Seq[Seq[Seq[Int]]]], tput : Double ) extends
    NNLayer( math.ceil( tput ), weights(0)(0)(0).size * weights(0)(0).size * weights(0).size,
      weights.size, math.ceil( tput ).toInt )  {

  io.dataIn.ready := io.dataOut.ready

  val tPutRounded = math.ceil( throughput ).toInt

  val dataVec = inIOToVVV( weights(0)(0).size, weights(0)(0)(0).size )
  val convRes = ( 0 until tPutRounded ).map( idx => {
    val pConv = Module( new ParrallelTriConvSum( dtype, weights ) )
    pConv.io.dataIn := Vec( ( 0 until weights(0).size ).map( wIdx => {
      dataVec( wIdx + weights(0).size * idx )
    }))
    ( pConv.io.dataOut, pConv.latency )
  })

  val convLat = convRes.map( _._2 ).max

  val convOut = convRes.map( o => {
    ShiftRegister( o._1, convLat - o._2 )
  }).reduce( (a, b) => Vec( a ++ b ) )

  val convValid = ShiftRegister( io.dataIn.valid, convLat, false.B, true.B )

  var muxLat = 0
  val noToMux = ( 1 / tput ).toInt
  if ( tput < 1 ) {
    assert( noToMux * tput == 1, "Must be integer" )
    assert( noToMux == 1 << log2Ceil( noToMux.toInt ), "Must be power of 2 to mux" )
    val cntrCmp = Wire( false.B.cloneType )
    val cntr = Counter( io.dataIn.valid | cntrCmp, noToMux )
    cntrCmp := cntr._1 > 0.U
    io.dataIn.ready := !io.dataIn.valid & !cntrCmp
    when ( cntr._1 === ( noToMux - 1 ).U ) {
      io.dataIn.ready := io.dataOut.ready
    }
    val grpSize = convOut.size / noToMux
    val convGrp = convOut.grouped( grpSize ).toSeq.map( Vec( _ ) )
    val mapping = convGrp.zipWithIndex.map( x => { ( x._2.U, x._1 ) } )
    val cntrDelay = ShiftRegister( cntr._1, convLat )
    val convMux = MuxLookup( cntrDelay, convGrp.head, mapping )
    val convReg = Reg( convOut.cloneType )
    for ( i <- 0 until noToMux ) {
      when ( cntrDelay === i.U ) {
        for ( j <- 0 until grpSize )
          convReg( i * grpSize + j ) := convMux( j )
      }
    }
    muxLat = noToMux

    io.dataOut.bits := convReg
    val vldOut = ShiftRegister( convValid, muxLat, false.B, true.B )
    io.dataOut.valid := vldOut
  } else {
    io.dataOut.bits := convOut
    io.dataOut.valid := convValid
  }
  // find the max latency
  val latency = convLat + muxLat
}
