
package binconcifar

import chisel3._
import chisel3.util._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Vgg7[ T <: SInt]( dtype : T ) extends Module {

  val tPut = 1.0
  val tPutPart1Int = math.max( tPut, 1 ).toInt
  val tPutOut = 1 //tPut / 4
  val imgSize = 32
  val imgOutSize = imgSize / 8
  // val dtype = SInt( 16.W )
  // type T = dtype.type
  val fracBits = 4
  val abFracBits = 6
  val inGrp = 3
  val noOut = 256
  val latency = 32 // just some bs for now
  val noIn = tPutPart1Int

  val io = IO(new Bundle {
    val dataIn : DecoupledIO[Vec[T]] = Flipped(Decoupled( Vec( tPutPart1Int * inGrp, dtype ) ))
    val dataOut : DecoupledIO[Vec[T]] = Decoupled( Vec( tPutOut * noOut, dtype.cloneType ) )
  })

  def createConvLyr(
    idx : Int,
    inputVec : DecoupledIO[Vec[T]],
    tPutLyr : Double,
    imgSize : Int,
    noFilt : Int,
    outFormat : ( Int, Int, Int ),
    noFifo : Boolean = false
  ) : DecoupledIO[Vec[T]] = {

    val bufferedSource = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_weights.csv" )
    val weights_raw = bufferedSource.getLines.toList
    val weights = weights_raw.map( _.split(",").toList.map( x => {
      x.toInt
    }) ).grouped( outFormat._3 ).toList.grouped( outFormat._2 ).toList
    val weights_trans = ( 0 until noFilt ).toList.map( i =>
      weights.map( w0 => w0.map( w1 => w1.map( w2 => w2(i) ) ) )
    ).map( x => x.map( _.reverse ).reverse )

    val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_ab.csv")
    val ab_raw = bufferedSource_ab.getLines.toList
    val ab = ab_raw.map( _.split(",").toList.map( x => math.round( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

    val tPutInt = math.max( tPutLyr, 1 ).toInt
    val blMod = Module( new SimpleBufferLayer( dtype, imgSize, outFormat._3, outFormat, 10, 1, true, tPutInt, noFifo = noFifo ) )
    val conv1 = Module( new TriConvSum( dtype, weights_trans, tPutLyr ) )

    val scaleShift = Module( new ScaleAndShift(
      dtype,
      fracBits,
      abFracBits,
      ab(0)take( noFilt ),
      ab(1).take( noFilt ),
      tPutInt
    ) )

    blMod.io.dataIn <> inputVec
    conv1.io.dataIn <> blMod.io.dataOut
    scaleShift.io.dataIn <> conv1.io.dataOut
    scaleShift.io.dataOut
  }

  def createPoolLyr(
    inputVec : DecoupledIO[Vec[T]],
    tPutLyr : Int,
    imgSize : Int,
    kernelFormat : ( Int, Int, Int )
  ) : DecoupledIO[Vec[T]] = {

    val blMod = Module( new SimpleBufferLayer( dtype, imgSize, kernelFormat._3, kernelFormat, 2, 2, false, tPutLyr, noFifo = true ) )
    val tPutPool = math.max( tPutLyr / 2, 1 ).toInt // divide by stride
    val poolMod = Module( new PoolLayer( dtype, tPutPool, kernelFormat ) )
    blMod.io.dataIn <> inputVec
    poolMod.io.dataIn <> blMod.io.dataOut
    val outIo = {
      if ( tPutPool > 1 )
        spreadStride( poolMod.io.dataOut, imgSize / 2 )
      else
        poolMod.io.dataOut
    }
    outIo
  }

  def halveTPut(
    inputVec : DecoupledIO[Vec[T]]
  ) : DecoupledIO[Vec[T]] = {
    val regFlag = RegInit( false.B )
    val secondHalf = Vec( inputVec.bits.take( inputVec.bits.size / 2 ) )
    val firstHalf = Vec( inputVec.bits.drop( inputVec.bits.size / 2 ) )
    val dcpOut = Wire( Decoupled( firstHalf.cloneType ) )
    val storeRes = inputVec.valid & dcpOut.ready
    val delayedHalf = RegEnable( firstHalf, storeRes )
    when( storeRes ) {
      regFlag := true.B
    }
    when ( regFlag & dcpOut.ready ) {
      regFlag := false.B
    }
    dcpOut.valid := inputVec.valid | regFlag
    dcpOut.bits := secondHalf
    when ( regFlag ) {
      dcpOut.bits := delayedHalf
    }
    inputVec.ready := dcpOut.ready & !regFlag
    dcpOut
  }

  def spreadStride(
    inputVec : DecoupledIO[Vec[T]],
    queueSize : Int
  ) : DecoupledIO[Vec[T]] = {
    val padQueue = Queue( inputVec, queueSize )
    halveTPut( padQueue )
  }

  def reverseOrder(
    inputVec : DecoupledIO[Vec[T]],
    tPutLyr : Int
  ) : DecoupledIO[Vec[T]] = {
    val noFilt = inputVec.bits.size / tPutLyr
    val bitsGrp = inputVec.bits.toList.grouped( noFilt ).toList.reverse
    val bitsOut = Vec( bitsGrp.reduce( _ ++ _ ) )
    val dcpOut = Wire( Decoupled( bitsOut.cloneType ) )
    dcpOut.valid := inputVec.valid
    dcpOut.bits := bitsOut
    inputVec.ready := true.B
    dcpOut
  }

  val lyr1 = createConvLyr( 1, io.dataIn, tPut, imgSize, 64, ( 3, 3, 3 ), true )
  val lyr1Rev = reverseOrder( lyr1, tPutPart1Int )
  val lyr2 = createConvLyr( 2, lyr1Rev, tPut, imgSize, 64, ( 3, 3, 64 ), true )
  val lyr2Rev = reverseOrder( lyr2, tPutPart1Int )
  val mp1 = createPoolLyr( lyr2Rev, tPutPart1Int, imgSize, ( 2, 2, 64 ) )

  // io.dataOut <> mp1

  val tPutPart2 = tPut / 4
  val tPutPart2Int = math.max( tPutPart2, 1 ).toInt
  val mp1Rev = reverseOrder( mp1, tPutPart2Int )
  val imgSizePart2 = imgSize / 2

  val lyr3 = createConvLyr( 3, mp1Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 64 )  )
  val lyr3Rev = reverseOrder( lyr3, tPutPart2Int )
  val lyr4 = createConvLyr( 4, lyr3Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 128 ) )
  val lyr4Rev = reverseOrder( lyr4, tPutPart2Int )
  val mp2 = createPoolLyr( lyr4Rev, tPutPart2Int, imgSizePart2, ( 2, 2, 128 ) )

  // io.dataOut <> mp2

  val tPutPart3 = tPutPart2 / 4
  val tPutPart3Int = math.max( tPutPart3, 1 ).toInt
  val mp2Rev = reverseOrder( mp2, tPutPart3Int )
  val imgSizePart3 = imgSize / 4

  val lyr5 = createConvLyr( 5, mp2Rev, tPutPart3, imgSizePart3, 256, ( 3, 3, 128 )  )
  val lyr5Rev = reverseOrder( lyr5, tPutPart3Int )
  val lyr6 = createConvLyr( 6, lyr5Rev, tPutPart3, imgSizePart3, 256, ( 3, 3, 256 ) )
  val lyr6Rev = reverseOrder( lyr6, tPutPart3Int )
  val mp3 = createPoolLyr( lyr6Rev, tPutPart3Int, imgSizePart3, ( 2, 2, 256 ) )
  io.dataOut <> mp3
}
