
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

  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  def lcm( a : Int, b : Int ) : Int = a * ( b / gcd( a, b ) )
}

/** This class buffers an image to the correct size
  * It streams in an image of "imgSize x imgSize x grpSize" and outputs blocks of outFormat
  * every "stride" pixels with or without padding as some throughput "tPut"
  */
class BufferLayer( val imgSize : Int, val grpSize : Int, val outFormat : (Int, Int, Int), qSize : Int,
  val stride : Int, val padding : Boolean, tPut : Double ) extends NNLayer( tPut, grpSize,
    outFormat._1 * outFormat._2 * outFormat._3, BufferLayer.calcOutSize( stride, tPut ) ) {

  Predef.assert( outFormat._1 == outFormat._2, "Must be square conv for now" )

  val debug = true

  // calculate the vlds and padding
  val padSize_1 = ( outFormat._1 - 1 )/ 2
  val padSize_2 = ( outFormat._2 - 1 )/ 2

  val bufferSize = {
    if ( throughput < 1 )
      imgSize
    else
      ( imgSize / throughput ).toInt
  }

  val remainder = ( imgSize - bufferSize * throughput ).toInt
  println( "remainder = " + remainder )

  // calculate the number of inputs for image loops
  val noImgPos = BufferLayer.lcm( imgSize * imgSize, noIn ) / noIn
  println( "noImgPos = " + noImgPos )

  // wait until init done until start sending
  val initCount = {
    if ( padding )
      padSize_1 * imgSize + padSize_2
    else
      ( outFormat._1 - 1 ) * imgSize + ( outFormat._2 - 1 )
  }

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

  if ( debug )
    printf( "nextData = %d\n", nextData )

  /** Count "cnt" vld's and then output true
    */
  def initFlagCount( vld : Bool, cnt : Int ) : Bool = {
    val vldCnt = Counter( vld, cnt )
    val doneFlag = RegInit( false.B )
    when ( vldCnt._2 ) {
      doneFlag := true.B
    }
    doneFlag
  }

  /* This method buffers image rows
   */
  def getMemBuffers( inputData : Vec[T], nextData : Bool ) : List[(Vec[T], Bool)] = {

    val memBuffers = ArrayBuffer[(Vec[T], Bool)]()
    memBuffers += { ( inputData, nextData ) } // this is the most recent row

    // create a mem to buffer the data to increase the latency to the correct amount
    while ( memBuffers.size < outFormat._1 ) {
      val mb = memBuffers.last
      printf( "mb( %d ) = ", mb._2 )
      for ( d <- mb._1 )
        printf( "%d, ", d )
      printf( "\n" )
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
      printf( "chosenVec( %d ) = ", mb._2 )
      for ( v <- chosenVec )
        printf( "%d, ", v )
      printf( "\n" )
      // finally feed into the next membuffer
      val mbData = ShiftRegister( chosenVec, bufferSize, mb._2 ) // MemShiftRegister( chosenVec, bufferSize, mb._2 )
      val vldSr = nextData & initFlagCount( mb._2, bufferSize )
      printf( "mbData( %d ) = ", vldSr )
      for ( d <- mbData )
        printf( "%d, ", d )
      printf( "\n" )
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

  /** This method buffers an individual row output stream into windowed columns
    */
  def bufferVec( vecIn : Vec[T], vld : Bool, windShift : UInt, stride : Int,
    noIgnore : Int, displayBefore : Int ) : (Vec[T], Bool) = {
    // consider stride and throughput ( is vec size )

    val sldWin = Module( new SlidingWindow( dtype.cloneType, outFormat._3, noIn,
      outFormat._2, stride, noIgnore, displayBefore ) )
    printf( "sldWin.dataIn( %d ) = ", vld )
    for ( v <- vecIn )
      printf( "%d, ", v )
    printf( "\n" )
    sldWin.io.dataIn.bits := vecIn
    sldWin.io.windShift := windShift
    sldWin.io.dataIn.valid := vld

    ( sldWin.io.dataOut.bits, sldWin.io.dataOut.valid )
  }

  val padSize = {
    if ( padding )
      padSize_2
    else
      0
  }

  // for each row, get the column buffer
  val joinedVecs = memBuffers.zipWithIndex.map( z => {
    val noIgnore = (z._2 * remainder + {
      val startIdx = memBuffers.size - 1 - padSize - z._2
      if ( startIdx < 0 )
        ( stride + startIdx ) * imgSize // to the next output if padding
      else
        startIdx * imgSize
    }) % stride
    val windShift = 0
    println( "noIgnore = " + noIgnore )
    println( "windShift = " + windShift )
    bufferVec( z._1._1, z._1._2, windShift.U, stride, noIgnore, padSize )
  })
  // put each output together
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

  // count the position in the image
  val imgCntr = Counter( nextData, noImgPos )

  val initDone = RegInit( false.B )
  val initCntr = Counter( nextData, ( initCount.toDouble / noIn ).toInt )
  val initMasks = Wire( false.B.cloneType )
  initMasks := false.B
  when ( nextData & initCntr._2 ) {
    initDone := true.B
    initMasks := !initDone
  }
  val initMasksDone = Reg( Vec( noOut, false.B.cloneType ) )
  for ( i <- 0 until noOut ) {
    when ( nextData ) {
      initMasksDone( i ) := true.B
    }
    if ( i < initCount % noOut ) {
      when ( nextData & initMasks ) {
        initMasksDone( i ) := false.B
      }
    }
  }

  val img = ArrayBuffer[ArrayBuffer[Boolean]]()

  /** This method keeps track of the image as a whole and sets the valid flags
    */
  def getValid( nxt : Bool ) : ( Bool, Vec[Bool] ) = {
    val padOffset_1 = stride - {
      if ( padding )
        padSize_1 % stride
      else
        ( outFormat._1 - 1 ) % stride
    }
    val padOffset_2 = stride - {
      if ( padding )
        padSize_2 % stride
      else
        ( outFormat._2 - 1 ) % stride
    }

    while( img.size < imgSize )
      img += ArrayBuffer.fill( imgSize ) { true }
    for ( i <- 0 until imgSize ) {
      for ( j <- 0 until imgSize ) {
        // set strides to 0
        val iMod = i + padOffset_1 + {
          // look at prev img
          if ( padding && ( i < padSize_1 || ( i <= padSize_1 && j < padSize_2 ) ) )
            imgSize
          else
            0
        }
        val jMod = j + padOffset_2
        if ( j >= padSize_2 && padding || j >= outFormat._2 - 1 ) {
          if ( iMod % stride != 0 || jMod % stride != 0 )
            img( i )( j ) = false
        }
        if ( j < outFormat._2 - 1 && !padding )
          img( i )( j ) = false
        if ( j < padSize_2 && padding ) {
          if ( ( iMod - 1 ) % stride != 0 ||
            ( jMod + imgSize ) % stride != 0 )
            img( i )( j ) = false
        }
      }
    }
    println( "imgVld = " + img )
    // the vld masks when there is more than one output
    val imgMasks = ( 0 until noImgPos ).map( i => {
      val posRng = ( ( i * noIn ) until ( i + 1 ) * noIn ).toList
      val outGrpSize = ( noIn / noOut )
      val imgPos = posRng.map( z => {
        ( z / imgSize, z % imgSize )
      }).grouped( outGrpSize ).toList
      imgPos.map( grp => {
        grp.map( z => {
          val x = z._1
          val y = z._2
          img( x % imgSize )( y )
        }).reduce( _ || _ )
      })
    }).toList
    println( "imgMasks = " + imgMasks )
    val imgVld = imgMasks.map( z => z.reduce( _ || _ ) )
    val imgMasksVec = Vec( imgMasks.map( x => Vec( x.map( _.B ) )) )
    val imgVldVec = Vec( imgVld.map( _.B ) )

    // vld outputs
    val vldOut = ShiftRegister( imgVldVec( imgCntr._1 ) & initDone, 1, false.B, nxt )
    val imgMasksOut = Wire( Vec( noOut, false.B.cloneType ) )
    for ( i <- 0 until noOut )
      imgMasksOut( i ) := imgMasksVec( imgCntr._1 )( i ) & initMasksDone( i )

    val vldMsk = ShiftRegister( imgMasksOut, 1, nxt )
    ( vldOut & nxt, vldMsk )
  }

  val padVals = ArrayBuffer[ArrayBuffer[(Int, Int)]]()

  /** This method watches the images as a whole an masks numbers which are not valid when used with padding
    */
  def getPadding( nxt : Bool ) : ( Vec[SInt], Vec[SInt] ) = {

    while( padVals.size < imgSize )
      padVals += ArrayBuffer.fill( imgSize ) { ( 0, 0 ) } // default to no padding

    for ( x <- 0 until imgSize ) {
      for ( y <- 0 until imgSize ) {
        val xMod = {
          if ( y < padSize_2 )
            ( x - 1 + imgSize ) % imgSize
          else
            x
        }
        // set up top padding
        if ( xMod < padSize_1 ) // from the previous image
          padVals( x )( y ) = ( - xMod - 1, padVals( x )( y )._2 )
        else if ( xMod < outFormat._1 ) // first rows of this image
          padVals( x )( y ) = ( outFormat._1 - 1 - xMod, padVals( x )( y )._2 )

        if ( y < padSize_2 ) // set up previous row padding
          padVals( x )( y ) = ( padVals( x )( y )._1, - y - 1 )
        else if ( y < outFormat._2 ) // set up this rows padding
          padVals( x )( y ) = ( padVals( x )( y )._1, outFormat._2 - 1 - y )
      }
    }
    println( "padVals = " )
    for ( v <- padVals )
      println( v )
    val padMaskIdxs = ( 0 until noImgPos ).map( i => {
      val posRng = ( ( i * noIn ) until ( i + 1 ) * noIn ).toList
      posRng.filter( z => {
        val zMod = ( z - padSize_1 * imgSize - padSize_2 + imgSize * imgSize ) % ( imgSize * imgSize )
        val zModY = zMod % stride
        val zModX = ( zMod / imgSize ) % stride
        zModX == 0 && zModY == 0
      })
    })
    println( "padMasksIdxs = " + padMaskIdxs )
    val padMask = padMaskIdxs.map( imgPos => {
      val padXY = imgPos.map( z => {
        ( z / imgSize, z % imgSize )
      }).map( z => {
        val y = z._2
        val x = z._1
        padVals( x % imgSize )( y )
      })
      ( padXY.map( _._1 ), padXY.map( _._2 ) )
    }).toList
    println( "padMask = " + padMask )
    val padMaskVecX = Wire( Vec( noImgPos, Vec( noOut, 0.S( log2Ceil( outFormat._1 ).W ).cloneType ) ) )
    val padMaskVecY = Wire( Vec( noImgPos, Vec( noOut, 0.S( log2Ceil( outFormat._2 ).W ).cloneType ) ) )
    for ( i <- 0 until noImgPos ) {
      for ( j <- 0 until noOut ) {
        if ( j < padMask( i )._1.size ){
          padMaskVecX( i )( j ) := padMask( i )._1( j ).S
          padMaskVecY( i )( j ) := padMask( i )._2( j ).S
        } else {
          // change to dont care?
          padMaskVecX( i )( j ) := 0.S
          padMaskVecY( i )( j ) := 0.S
        }
      }
    }

    val padMaskXOut = ShiftRegister( padMaskVecX( imgCntr._1 ), 1, nxt )
    val padMaskYOut = ShiftRegister( padMaskVecY( imgCntr._1 ), 1, nxt )

    ( padMaskXOut, padMaskYOut )
  }

  val vldRes = getValid( nextData )

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

  // apply padding to the output vector
  val dataJoined : Vec[T] = {
    if ( padding ) {
      val padRes = getPadding( nextData )

      val padXY = padRes._1.zip( padRes._2 )
      printf( "padXY = " )
      for ( xy <- padXY )
        printf( " (%d, %d), ", xy._1, xy._2 )
      printf( "\n" )

      val newDataOut = dataOut.zip( padXY.reverse ).map( convImg => {
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
            val condXY = RegEnable( condX | condY, nextData )
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
  io.dataOut.valid := RegEnable( vldRes._1, false.B, true.B )
  io.vldMask := RegEnable( vldRes._2, true.B )

}
