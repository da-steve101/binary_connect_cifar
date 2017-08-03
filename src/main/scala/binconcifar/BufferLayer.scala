
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

class BufferLayer( val imgSize : Int, grpSize : Int, val outFormat : (Int, Int, Int), qSize : Int,
  stride : Int, padding : Boolean, tPut : Double ) extends NNLayer( tPut, grpSize,
    outFormat._1 * outFormat._2 * outFormat._3, BufferLayer.calcOutSize( stride, tPut ) ) {

  Predef.assert( outFormat._1 == outFormat._2, "Must be square conv for now" )

  val debug = true

  // Take the input and make it have a throughput of 1 to make life easier
  val bufferedInput = {
    if ( throughput < 1 ) {
      val mySer = Serializer( dtype, grpSize, (grpSize/throughput).toInt )
      val dcpIO = Wire( Decoupled( mySer.io.dataOut.bits ) )
      dcpIO.bits := mySer.io.dataOut.bits
      dcpIO.valid := mySer.io.dataOut.valid
      dcpIO
    } else
      io.dataIn
  }

  // Buffer the input
  val inQueue = Queue( bufferedInput, qSize )

  val ready = io.dataOut.ready
  inQueue.ready := ready
  val nextData = inQueue.valid & ready

  // get a counter that increments by 'inc'
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

  // buffer the image rows
  def getMemBuffers( inputData : Vec[T], bufferSize : Int, nextData : Bool ) : List[(Vec[T], Bool)] = {
    val remainder = ( imgSize - bufferSize * throughput ).toInt

    val memBuffers = ArrayBuffer[(Vec[T], Bool)]()
    memBuffers += { ( inputData, nextData) }// this is the most recent row

    // create a mem to buffer the data to increase the latency to the correct amount
    while ( memBuffers.size < outFormat._1 ) {
      val mb = memBuffers.last
      // take the output of mb
      val lastInput = mb._1.grouped( outFormat._3 ).toList.map( Vec( _ ) )
      // delay the input by one cycle
      val lastReg = lastInput.map( x => RegEnable( x, mb._2 ) )
      // combine together
      val lastComb = lastReg ++ lastInput
      // pick the shifted data so it aligns with the imgSize
      val chosenVec = ( 0 until lastInput.size ).map( i => {
        lastComb( lastInput.size + i - remainder )
      }).reduce( ( a, b ) => Vec( a ++ b ) )
      // finally feed into the next membuffer
      val mbData = MemShiftRegister( chosenVec, bufferSize, mb._2 )
      // shift valid with it
      val vldSr = ShiftRegister( mb._2, bufferSize, false.B, true.B )
      memBuffers += { ( mbData, vldSr ) }
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
    printf( "sldWin.dataIn( %d ) = ", vld )
    for ( v <- vecIn )
      printf( "%d, ", v )
    printf( "\n" )
    sldWin.io.dataIn.bits := vecIn
    sldWin.io.dataIn.valid := vld

    ( sldWin.io.dataOut.bits, sldWin.io.dataOut.valid )
  }

  val joinedVecs = memBuffers.map( mb => bufferVec( mb._1, mb._2, stride ) )
  val grpedVecs = joinedVecs.map( _._1 ).map( jv => jv.grouped( outFormat._2*outFormat._3 ).toList )
  val dataOut = ( 0 until noOut ).map( i => { grpedVecs.map( jv => jv( i ) ) } ).toList

  if ( debug ) {
    for ( mb <- memBuffers.zipWithIndex ) {
      printf( "memBuffers( " + mb._2 + " )( %d ) = ", mb._1._2 )
      for ( v <- mb._1._1 )
        printf( "%d, ", v )
      printf( "\n" )
    }
    for ( mb <- joinedVecs.map( _._1 ).zipWithIndex ) {
      printf( "joinedVecs( " + mb._2 + " ) = " )
      for ( v <- mb._1 )
        printf( "%d, ", v )
      printf( "\n" )
    }
  }

  io.dataOut.bits := Vec( dataOut.map( jv => jv.reduce( _ ++ _ ) ).reverse.reduce( _ ++ _ ) ) 

  def getValid( nxt : Bool ) : ( Bool, Vec[Bool] ) = {
    // calculate the number of inputs in an image
    val noImgPos = math.ceil( imgSize*imgSize.toDouble / noOut ).toInt
    val imgCntr = Counter( nxt, noImgPos )

    val padSize = ( outFormat._2 - 1 )/ 2
    // the vld positions when finishing a conv in a row
    val imgRow = List.fill( outFormat._2 - 1 ) { false } ++ List.fill( imgSize - outFormat._2 + 1  ) { true }
    // the vld positions when finishing a conv for the whole image
    val img = List.fill( outFormat._1 - 1 ) { List.fill( imgSize ) { false } } ++ List.fill( imgSize - outFormat._1 + 1 ) { imgRow }
    // the vld masks when there is more than one output
    val imgMasks = ( 0 until noImgPos ).map( i => {
      val posRng = ( ( i * noOut ) until ( i + 1 )*noOut ).toList
      val imgPos = posRng.map( z => {
        ( z / imgSize, z % imgSize )
      })
      imgPos.map( z => {
        val x = z._1
        val y = z._2
        val r = {
          if ( x < imgSize )
            img( x )( y )
          else
            false
        }
        r
      })
    }).toList
    val imgVld = imgMasks.map( z => z.reduce( _ || _ ) )
    val imgMasksVec = Vec( imgMasks.map( x => Vec( x.map( _.B ) )) )
    val imgVldVec = Vec( imgVld.map( _.B ) )

    // vld outputs
    val vldOut = ShiftRegister( imgVldVec( imgCntr._1 ), 2, false.B, nxt )
    val vldMsk = ShiftRegister( imgMasksVec( imgCntr._1 ), 2, nxt )

    ( vldOut, vldMsk )
  }

  val vldCntr = getValid( nextData )

  io.dataOut.valid := vldCntr._1
  io.vldMask := vldCntr._2

}
