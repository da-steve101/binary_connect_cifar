
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object BufferLayer {
  // cases to consider, stride = 1, tPut = 2, stride = 2, tPut = 3, stride = 2, tPut = 4
  def calcOutSize( stride : Int, tPut : Double ) : Int = {
    if ( tPut >= math.pow( stride, 2 ) )
      ( tPut / math.pow( stride, 2 ) ).toInt
    else
      1
  }
}

class BufferLayer( val imgSize : Int, inSize : Int, val outFormat : (Int, Int, Int), qSize : Int,
  stride : Int, padding : Boolean, tPut : Double ) extends NNLayer( tPut, inSize,
    outFormat._1 * outFormat._2 * outFormat._3, BufferLayer.calcOutSize( stride, tPut ) ) {

  Predef.assert( outFormat._1 == outFormat._2, "Must be square conv for now" )

  val debug = true

  val bufferedInput = {
    if ( throughput < 1 ) {
      val mySer = Serializer( dtype, inSize, (inSize/throughput).toInt )
      val dcpIO = Wire( Decoupled( mySer.io.dataOut.bits ) )
      dcpIO.bits := mySer.io.dataOut.bits
      dcpIO.valid := mySer.io.dataOut.valid
      dcpIO
    } else
      io.dataIn
  }

  /* Buffer up the input to give a square
   */
  val inQueue = Queue( bufferedInput, qSize )

  val ready = io.dataOut.ready
  inQueue.ready := ready
  val nextData = inQueue.valid & ready

  def zeroedSR( n : Int, b : Bool ) : Bool = {
    val zeroCntr = Counter( true.B, n - 1 )
    val initDone = RegInit( false.B )
    initDone := ( initDone | zeroCntr._2 )
    val sr = ShiftRegister( b, n )
    initDone | sr
  }

  def getCounter( nxt : Bool, size : Int, inc : Int ) : ( UInt, Bool ) = {
    val bw = math.ceil( math.log( size + inc )/math.log( 2 ) ).toInt
    val cntrReg = RegInit( 0.U( bw.W ) )
    val wrap = Wire( Bool() )
    val nxtCnt = cntrReg + inc.U
    wrap := ( nxtCnt >= size.U ) && nxt
    when ( nxt ) {
      cntrReg := nxtCnt
    }
    when ( wrap ) {
      cntrReg := nxtCnt - size.U
    }
    ( cntrReg, wrap )
  }

  def getValid( nxt : Bool ) : ( Bool, Bool ) = {
    val cntrInc = math.ceil(throughput).toInt
    val colCntr = getCounter( nxt, imgSize, cntrInc )
    val rowCntr = Counter( colCntr._2, imgSize )
    val vldOut = Wire( Bool() )
    val vldMsk = Wire( Vec( noOut, Bool() ) )

    if ( debug ) {
      printf( "rowCntr = %d\n", rowCntr._1 )
      printf( "colCntr = %d\n", colCntr._1 )
      printf( "vldOut = %d\n", vldOut )
      printf( "vldMask = " )
      for ( d <- vldMsk )
        printf( "%d, ", d )
      printf( "\n" )
    }
    val padSize = ( outFormat._2 - 1 )/ 2
    val vldDist = math.max( padSize, noOut )
    val vldSelVec = Vec(
      List.fill( noOut ) { true.B } ++ // prev row
      List.fill( 2*padSize ) { false.B } ++ // end of prev row and start of this one
      List.fill( imgSize - 2*padSize ) { true.B } ++ // middle of row
      List.fill( 2*padSize ) { false.B } ++ // end of row and start of next one
      List.fill( noOut - 2*padSize ) { true.B } // middle of next row
    )
    val padBw = math.ceil( math.log( imgSize + noOut + padSize ) / math.log(2) ).toInt
    val highNum = Wire( UInt( padBw.W ) )
    val lowNum = Wire( UInt( padBw.W ) )
    highNum := colCntr._1 + ( noOut + padSize ).U
    lowNum := colCntr._1 + padSize.U
    vldOut := nxt
    val colMin = math.max( 2*padSize - noOut, 0 )
    for ( i <- 0 until noOut )
      vldMsk( i ) := true.B
    if ( padding ) {
      vldOut := true.B
    } else {
      when ( colCntr._1 < colMin.U ||
        colCntr._1 >= ( imgSize - padSize + noOut - 1 ).U ) {
        vldOut := false.B
        // printf( "colCntr._1 = %d means false\n", colCntr._1 )
      }
      DynamicVecAssign( vldMsk, ( noOut - 1 ).U, 0.U,
        vldSelVec, highNum, lowNum )
      when ( rowCntr._1 < padSize.U ||
        rowCntr._1 >= ( imgSize - padSize ).U ) {
        vldOut := false.B
      }
    }

    val latency = math.ceil( ( padSize * imgSize + outFormat._1 - padSize )/ throughput ).toInt

    for ( i <- 0 until noOut )
      io.vldMask( i ) := ShiftRegister( vldMsk( i ), latency - 1 )

    ( zeroedSR( latency, vldOut ), colCntr._2 )
  }

  val vldCntr = getValid( nextData )
  // valid should be delayed using counter ...
  io.dataOut.valid := vldCntr._1

  if ( debug )
    printf( "nextData = %d\n", nextData )

  def getMemBuffers( inputData : Vec[T], bufferSize : Int, nextData : Bool ) : List[Vec[T]] = {
    val remainder = ( imgSize - bufferSize * throughput ).toInt

    val memBuffers = ArrayBuffer[Vec[T]]()
    memBuffers += inputData // this is the reference

    // create a mem to buffer the data to increase the latency to the correct amount
    while ( memBuffers.size < outFormat._1 ) {
      val lastInput = memBuffers.last.grouped( outFormat._3 ).toList.map( Vec( _ ) )
      val lastReg = lastInput.map( x => RegEnable( x, nextData ) )
      val lastComb = lastReg ++ lastInput
      val chosenVec = ( 0 until lastInput.size ).map( i => {
        lastComb( lastInput.size + i - remainder )
      }).reduce( ( a, b ) => Vec( a ++ b ) )
      memBuffers += MemShiftRegister( chosenVec, bufferSize, nextData )
    }

    memBuffers.toList
  }

  val bufferSize = {
    if ( throughput < 1 )
      imgSize
    else
      ( imgSize / throughput ).toInt
  }

  val memBuffers = getMemBuffers( inQueue.bits, bufferSize, nextData )

  override def latency : Int = {
    // can define it ...
    new ChiselExecutionFailure( "Cannot get latency for BufferLayer as Queue means it is undefined" )
    -1
  }

  /** Responsible for buffering a vec that has multiple outputs
    */
  def bufferVec( vecIn : Vec[T], stride : Int ) : List[Vec[Vec[T]]] = {
    // consider stride and throughput ( is vec size )

    // partition vecIn into its dims
    val vecGrp = vecIn.grouped( outFormat._3 ).toList.map( Vec( _ ) )
    val vecGrpReg = vecGrp.map( vg => { RegEnable( vg, nextData ) })
    val vecComb = vecGrpReg ++ vecGrp // combined vec of last 2 cycs

    val tmpRegInits = ( 0 until noOut ).map( idx => {
      ( 0 until noOut ).map( i => vecComb( vecGrp.size - idx + i ) ).toList.reverse
    })

    if ( debug ) {
      for ( tri <- tmpRegInits.zipWithIndex ) {
        printf( "tmpRegInit( " + tri._2 + " ) = " )
        for ( x <- tri._1 ) {
          for ( y <- x )
            printf( "%d, ", y )
        }
        printf( "\n" )
      }
    }

    // buffer the regs
    val bufferRegs = ( 0 until noOut ).map( idx => {
      val tmpRegs = List.fill( outFormat._2 ) { List.fill( outFormat._3 ) { Reg( dtype.cloneType ) } }
      when ( nextData ) {
        for ( i <- 0 until noOut ) {
          for ( j <- 0 until outFormat._3 )
            tmpRegs( i )( j ) := tmpRegInits( idx )( i )( j )
        }
        for ( i <- 0 until outFormat._2 - noOut ) {
          for ( j <- 0 until outFormat._3 )
            tmpRegs( i + noOut )( j ) := tmpRegs( i )( j )
        }
      }
      if ( debug ) {
        for ( a <- tmpRegs.zipWithIndex ) {
          printf( "tmpRegs( " + idx + " )( " + a._2 + " ) = " )
          for ( b <- a._1 )
            printf( "%d,", b )
          printf( "\n" )
        }
      }

      Vec( tmpRegs.map( Vec( _ ) ) )
    }).toList

    bufferRegs
  }

  val bufferedVecs = memBuffers.map( mb => bufferVec( mb, stride ) )
  val outVecs = ( 0 until bufferedVecs(0).size ).toList.map( i => bufferedVecs.map( x => x(i) ) )
  val joinedVecs = outVecs.map( x => x.reduce( ( a, b ) => Vec( a ++ b ) ).reverse.reduce( ( a, b ) => Vec( a ++ b ) ) )

  if ( debug ) {
    for ( mb <- memBuffers.zipWithIndex ) {
      printf( "memBuffers( " + mb._2 + " ) = " )
      for ( v <- mb._1 )
        printf( "%d, ", v )
      printf( "\n" )
    }
  }

  io.dataOut.bits := joinedVecs.reverse.reduce( ( a, b ) => Vec( a ++ b ) )

}
