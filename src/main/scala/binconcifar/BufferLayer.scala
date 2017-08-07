
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

class BufferLayer( val imgSize : Int, val grpSize : Int, val outFormat : (Int, Int, Int), qSize : Int,
  val stride : Int, val padding : Boolean, tPut : Double ) extends NNLayer( tPut, grpSize,
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

  val bufferSize = {
    if ( throughput < 1 )
      imgSize
    else
      ( imgSize / throughput ).toInt
  }
  val remainder = ( imgSize - bufferSize * throughput ).toInt

  // buffer the image rows
  def getMemBuffers( inputData : Vec[T], nextData : Bool ) : List[(Vec[T], Bool)] = {

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
      val lastComb = lastInput ++ lastReg
      // pick the shifted data so it aligns with the imgSize
      val chosenVec = ( 0 until lastInput.size ).map( i => {
        lastComb( i + remainder )
      }).reduce( ( a, b ) => Vec( a ++ b ) )
      printf( "chosenVec = " )
      for ( v <- chosenVec )
        printf( "%d, ", v )
      printf( "\n" )
      // finally feed into the next membuffer
      val mbData = MemShiftRegister( chosenVec, bufferSize, mb._2 )
      // shift valid with it
      val vldSr = ShiftRegister( mb._2, bufferSize, false.B, true.B )
      memBuffers += { ( mbData, vldSr ) }
    }

    memBuffers.toList
  }


  val memBuffers = getMemBuffers( inQueue.bits, nextData )

  override def latency : Int = {
    // can define it ...
    new ChiselExecutionFailure( "Cannot get latency for BufferLayer as Queue means it is undefined" )
    -1
  }

  /** Responsible for buffering a vec that has multiple outputs
    */
  def bufferVec( vecIn : Vec[T], vld : Bool, stride : Int, bufferOffset : Int ) : (Vec[T], Bool) = {
    // consider stride and throughput ( is vec size )

    val sldWin = Module( new SlidingWindow( dtype.cloneType, outFormat._3, noIn, outFormat._2, stride, bufferOffset ) )
    printf( "sldWin.dataIn( %d ) = ", vld )
    for ( v <- vecIn )
      printf( "%d, ", v )
    printf( "\n" )
    sldWin.io.dataIn.bits := vecIn
    sldWin.io.dataIn.valid := vld

    ( sldWin.io.dataOut.bits, sldWin.io.dataOut.valid )
  }

  val buffOffs = ( 0 until memBuffers.size ).map( i => ( i*remainder ) % throughput.toInt ).toList
  val joinedVecs = memBuffers.zip( buffOffs ).map( z => bufferVec( z._1._1, z._1._2, stride, z._2 ) )
  val grpedVecs = joinedVecs.map( _._1 ).map( jv => {
    // group to rows
    jv.grouped( outFormat._2*outFormat._3).toList.map( grp => {
      grp.grouped( outFormat._3 ).toList // group to pixels
    })
  })
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

  /* encode paddings as:
   * 0 = no padding
   * 2 = pad top 2
   * 1 = pad top 1
   * -1 = pad bot 1
   * -2 = pad bot 2
   * etc ...
   */
  def encodePadding( scrIn : Int, scrSize : Int, rowWrap : Int = 1 ) : Int = {
    if ( scrIn >= 0 )
      0 // no padding
    else {
      val padNeeded = ( scrSize - 1 )/2
      val tmp = scrIn + padNeeded
      if ( tmp >= 0 )
        padNeeded - tmp
      else
        -( padNeeded + tmp ) - 1 + ( 1 - rowWrap )
    }
  }

  def getValid( nxt : Bool ) : ( Bool, Vec[Bool], Vec[SInt], Vec[SInt] ) = {
    // calculate the number of inputs in an image
    val noImgPos = math.ceil( imgSize*imgSize.toDouble / ( noOut * stride ) ).toInt
    val imgCntr = Counter( nxt, noImgPos )

    val padSize = ( outFormat._2 - 1 )/ 2
    // the vld positions when finishing a conv in a row
    val offsetDim1 = {
      if ( padding )
        ( outFormat._1 - 1 )/2  // issue here when img loops
      else
        outFormat._1 - 1
    }

    val offsetDim2 = {
      if ( padding )
        0
      else
        outFormat._2 - 1
    }

    val imgRow = List.fill( offsetDim2 ) { false } ++ List.fill( imgSize - offsetDim2  ) { true }
    val imgStart = List.fill( offsetDim1 ) { List.fill( imgSize ) { false } } ++ {
      if ( padding )
        List( List.fill( padSize ) { false } ++ List.fill( imgSize - padSize ) { true } )
      else
        List[List[Boolean]]()
    }
    // the vld positions when finishing a conv for the whole image
    val img =  imgStart ++ List.fill( imgSize - offsetDim1 ) { imgRow }
    // the vld masks when there is more than one output
    val imgMasks = ( 0 until noImgPos ).map( i => {
      val posRng = ( ( i * noOut ) until ( i + 1 )*noOut ).toList
      val imgPos = posRng.map( z => {
        val zStride = z * stride
        ( zStride / imgSize, zStride % imgSize )
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
    val vldOut = ShiftRegister( imgVldVec( imgCntr._1 ), 1, false.B, nxt )
    val vldMsk = ShiftRegister( imgMasksVec( imgCntr._1 ), 1, nxt )

    val padMask = ( 0 until noImgPos ).map( i => {
      val posRng = ( ( i * noOut ) until ( i + 1 )*noOut ).toList
      val imgPos = posRng.map( z => {
        val zStride = z * stride
        ( zStride / imgSize, zStride % imgSize )
      })
      val padXY = imgPos.map( z => {
        val y = z._2 - outFormat._2 + 1
        val rowWrap = {
          if ( z._2 < ( outFormat._2 - 1 ) / 2 )
            0
          else
            1
         }
        val x = z._1 - outFormat._1 + rowWrap
        val xPad = encodePadding( x, outFormat._1, rowWrap )
        val yPad = encodePadding( y, outFormat._2 )
        ( xPad, yPad )
      }).reverse
      ( padXY.map( _._1 ), padXY.map( _._2 ) )
    }).toList
    println( "padMask = " + padMask )
    val padMaskVecX = Wire( Vec( noImgPos, Vec( noOut, 0.S( log2Ceil( outFormat._1 ).W ).cloneType ) ) )
    val padMaskVecY = Wire( Vec( noImgPos, Vec( noOut, 0.S( log2Ceil( outFormat._2 ).W ).cloneType ) ) )
    for ( i <- 0 until noImgPos ) {
      for ( j <- 0 until noOut ) {
        padMaskVecX( i )( j ) := padMask( i )._1( j ).S
        padMaskVecY( i )( j ) := padMask( i )._2( j ).S
      }
    }

    val padMaskXOut = ShiftRegister( padMaskVecX( imgCntr._1 ), 1, nxt )
    val padMaskYOut = ShiftRegister( padMaskVecY( imgCntr._1 ), 1, nxt )

    ( vldOut, vldMsk, padMaskXOut, padMaskYOut )
  }

  val vldCntr = getValid( nextData )

  val zeroGrp = Wire( Vec( grpSize, dtype.cloneType ) )
  for ( i <- 0 until grpSize )
    zeroGrp( i ) := 0.S

  def getPadCond( x : Int, pX : SInt, xSize : Int ) : Bool = {
    val mp = ( xSize - 1 )/2
    if ( x < mp ) {
      ( pX > x.S ) // trigger when pX >= 1
    } else if ( x > mp ) {
      ( pX < ( x - xSize + 1 ).S )
    } else
        false.B
  }

  val padXY = vldCntr._3.zip( vldCntr._4 )
  printf( "padXY = " )
  for ( xy <- padXY )
    printf( " (%d, %d), ", xy._1, xy._2 )
  printf( "\n" )
  val dataJoined : Vec[T] = {
    if ( padding ) {
      val newDataOut = dataOut.zip( padXY ).map( convImg => {
        val padX = convImg._2._1
        val padY = convImg._2._2
        convImg._1.zipWithIndex.map( convRow => {
          val x = outFormat._1 - convRow._2 - 1
          convRow._1.zipWithIndex.map( convGrp => {
            val y = outFormat._2 - convGrp._2 - 1
            val grpVec = Wire( Vec( grpSize, dtype.cloneType ) )
            grpVec := convGrp._1
            val condX = getPadCond( x, padX, outFormat._1 )
            val condY = getPadCond( y, padY, outFormat._2 )
            printf( "condX( " + x + " )( " + y + " ) = %d\n", condX )
            printf( "condY( " + x + " )( " + y + " ) = %d\n", condY )
            val condXY = RegEnable( condX | condY, vldCntr._1 )
            when ( condXY ) {
              grpVec := zeroGrp
            }
            grpVec
          }).reduce( ( a, b ) => Vec( a ++ b ) )
        }).reduce( ( a, b ) => Vec( a ++ b ) )
      }).reverse.reduce( ( a, b ) => Vec( a ++ b ) )
      newDataOut
    } else {
      Vec( dataOut.map( jv => jv.map( p => p.reduce( _ ++ _ ) ).reduce( _ ++ _ ) ).reverse.reduce( _ ++ _ ) )
    }
  }

  io.dataOut.bits := dataJoined
  io.dataOut.valid := RegEnable( vldCntr._1, false.B, true.B )
  io.vldMask := RegEnable( vldCntr._2, true.B )

}
