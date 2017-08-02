
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
    ShiftRegister( b, n, false.B, true.B )
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
  def bufferVec( vecIn : Vec[T], vld : Bool, stride : Int ) : (Vec[T], Bool) = {
    // consider stride and throughput ( is vec size )

    val sldWin = Module( new SlidingWindow( dtype.cloneType, outFormat._3, noIn, outFormat._2, stride ) )
    sldWin.io.dataIn.bits := vecIn
    sldWin.io.dataIn.valid := vld

    ( sldWin.io.dataOut.bits, sldWin.io.dataOut.valid )
  }

  val joinedVecs = memBuffers.map( mb => bufferVec( mb, true.B, stride ) )
  val dataOut = joinedVecs.map( _._1 )

  if ( debug ) {
    for ( mb <- memBuffers.zipWithIndex ) {
      printf( "memBuffers( " + mb._2 + " ) = " )
      for ( v <- mb._1 )
        printf( "%d, ", v )
      printf( "\n" )
    }
  }

  io.dataOut.bits := dataOut.reduce( ( a, b ) => Vec( a ++ b ) )

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
    val imgRow = List.fill( outFormat._2 - 1 ) { false } ++ List.fill( imgSize - outFormat._2 + 1  ) { true }
    val vldMaskRaw = ( 0 until imgSize ).map( cnt => {
      ( 0 until noOut ).map( idx => {
        val imIdx = ( cnt + idx ) % imgSize
        imgRow( imIdx )
      }).toList
    }).toList
    val colOutRaw = vldMaskRaw.map( x => x.reduce( _ || _ ) )
    val vldMaskVec = Vec( vldMaskRaw.map( x => Vec( x.map( _.B ) )) )
    val colOutVec = Vec( colOutRaw.map( _.B ) )
    vldOut := nxt
    vldMsk := vldMaskVec( colCntr._1 )
    if ( padding ) {
      vldOut := true.B
    } else {
      when ( !colOutVec( colCntr._1 ) ) {
        vldOut := false.B
      }
      when ( rowCntr._1 < ( outFormat._1 - 1 ).U ) {
        vldOut := false.B
      }
    }
    for ( i <- 0 until noOut )
      io.vldMask( i ) := RegNext( RegNext( vldMsk( i ) ) )

    val vldReg = RegInit( false.B )
    vldReg := vldOut
    ( vldReg, colCntr._2 )
  }

  val vldCntr = getValid( nextData )
  // valid should be delayed using counter ...
  io.dataOut.valid := RegNext( vldCntr._1 )

}
