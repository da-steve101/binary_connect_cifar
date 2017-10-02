package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

/** This class buffers an image to the correct size
  * It streams in an image of "imgSize x imgSize x grpSize" and outputs blocks of outFormat
  * every "stride" pixels with or without padding as some throughput "tPut"
  */
class SimpleBufferLayer(
  val imgSize : Int,
  val grpSize : Int,
  val outFormat : (Int, Int, Int),
  qSize : Int,
  val stride : Int,
  val padding : Boolean,
  tPut : Int,
  debug : Boolean = false
) extends NNLayer(
  tPut.toDouble,
  grpSize,
  outFormat._1 * outFormat._2 * outFormat._3,
  math.ceil( tPut.toDouble / stride ).toInt
) {

  Predef.assert( outFormat._1 == outFormat._2, "Must be square conv" )
  val windowSize = outFormat._1

  // only works for windowSize = 3 and stride = 1 or windowSize = 2 and stride = 2
  Predef.assert( ( windowSize == 3 && stride == 1 && padding ) ||
    ( windowSize == 2 && stride == 2 && !padding ), "Only certain cases are supported" )
  Predef.assert( imgSize % stride == 0, "ImgSize must be divisible by stride" )
  Predef.assert( imgSize % tPut == 0, "ImgSize must be divisible by tPut" )

  val inQueue = Queue( io.dataIn, qSize )
  val ready = io.dataOut.ready
  val nextData = inQueue.valid & ready
  inQueue.ready := ready

  // just hacky compatibility for now ...
  for ( v <- io.vldMask )
    v := true.B

  def initCounter( vld : Bool, cnt : Int ) : Bool = {
    val vldCnt = Counter( vld, cnt )
    val doneFlag = RegInit( false.B )
    when ( vldCnt._2 ) {
      doneFlag := true.B
    }
    doneFlag
  }

  val cycPerRow = imgSize / tPut
  val latency = cycPerRow + 5

  def getMemBuffers( inputVec : ValidIO[Vec[T]], bufferSize : Int ) : List[ValidIO[Vec[T]]] = {
    val memBuffers = ArrayBuffer[ValidIO[Vec[T]]]()
    memBuffers += { inputVec }

    while ( memBuffers.size < outFormat._1 ) {
      val mb = memBuffers.last
      // TODO: replace with mem
      val output = Wire( inputVec.cloneType )
      output.bits := ShiftRegister( mb.bits, bufferSize, inputVec.valid )
      output.valid := inputVec.valid & initCounter( mb.valid, bufferSize )
      memBuffers += { output }
    }
    memBuffers.toList
  }

  val queueVld = Wire( Valid( inQueue.bits.cloneType ) )
  queueVld.valid := nextData
  queueVld.bits := inQueue.bits
  val memBuffers = getMemBuffers( queueVld, cycPerRow )

  def windowTheData(
    vecIn : ValidIO[Vec[T]],
    stride : Int
  ) : ValidIO[Vec[T]] = {
    val sldWin = Module( new SimpleSlidingWindow(
      dtype,
      grpSize,
      tPut,
      outFormat._2,
      stride
    ))
    sldWin.io.dataIn <> vecIn
    sldWin.io.dataOut
  }

  val windowedData = memBuffers.map( mb => {
    windowTheData( mb, stride )
  })

  val grpedVecs = windowedData.map( _.bits ).map( jv => {
    jv.grouped( outFormat._2 * outFormat._3 ).toList.map( grp => {
      grp.grouped( outFormat._3 ).toList
    })
  })

  val dataOut = ( 0 until noOut ).map( i => { grpedVecs.map( jv => jv( i ) ) } ).toList.reverse

  // is initialized when first and second window is valid for both cases
  val vld = windowedData.take( 2 ).map( _.valid ).reduce( _ && _ )

  val vldCycPerRow = ( imgSize / math.max( tPut, stride ) ).toInt

  val rowCntr = Counter( vld, vldCycPerRow )
  val colCntr = Counter( rowCntr._2, imgSize / stride )
  val lastCol = imgSize - 1
  val lastRow = vldCycPerRow - 1

  // need to do padding for window size = 3 and stride = 1
  if ( padding ) {
    val zeroGrp = Wire( Vec( grpSize, dtype.cloneType ) )
    for ( i <- 0 until grpSize )
      zeroGrp( i ) := 0.S

    val paddedData = dataOut.zipWithIndex.map( convImg => {
      val isFirst = convImg._2 == 0
      val isLast = convImg._2 == dataOut.size - 1
      convImg._1.zipWithIndex.map( convRow => {
        val isTop = convRow._2 == 0
        val isBot = convRow._2 == convImg._1.size - 1
        convRow._1.zipWithIndex.map( convGrp => {
          val isLeft = isFirst && convGrp._2 == convRow._1.size - 1
          val isRight = isLast && convGrp._2 == 0
          val grpVec = Wire( Vec( grpSize, dtype.cloneType ) )
          grpVec := convGrp._1
          if ( isTop || isBot || isLeft || isRight ) {
            val padConds : List[(Boolean, Bool)] = List(
              ( isRight, rowCntr._1 === lastRow.U ), // pad right
              ( isLeft, rowCntr._1 === 0.U ),        // pad left
              ( isBot, colCntr._1 === 0.U ),         // pad bot
              ( isTop, colCntr._1 === lastCol.U )    // pad top
            )
            val padIt = padConds.filter( _._1 ).map( _._2 ).reduce( _ || _ )
            when ( padIt ) {
              grpVec := zeroGrp
            }
          }
          grpVec.toList
        }).reduce( _ ++ _ )
      }).reduce( _ ++ _ )
    }).reduce( _ ++ _ )

    io.dataOut.bits := Vec( paddedData )
    io.dataOut.valid := vld
  } else {
    io.dataOut.bits := Vec( dataOut.reduce( _ ++ _ ).reduce( _ ++ _ ).map( _.toList ).reduce( _ ++ _ ) )
    // need to supress vld on odd strides
    val colCntrReg = {
      if ( tPut == 1 )
        RegEnable( !colCntr._1(0), false.B, nextData )
      else
        RegEnable( !colCntr._1(0), false.B, vld )
    }
    io.dataOut.valid := vld & colCntrReg
  }
}
