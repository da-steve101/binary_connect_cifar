
package binconcifar

import chisel3._
import chisel3.util._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Vgg7 extends Module {

  val tPut = 1
  val tPutOut = 1 //tPut / 4
  val imgSize = 16
  val imgOutSize = imgSize / 2
  val dtype = SInt( 16.W )
  val fracBits = 3
  val abFracBits = 5
  val inGrp = 64
  val noOut = 128
  val latency = 32 // just some bs for now
  val noIn = tPut

  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled( Vec( tPut * inGrp, dtype.cloneType ) ))
    val dataOut = Decoupled( Vec( tPutOut * noOut, dtype.cloneType ) )
  })

  def createConvLyr(
    idx : Int,
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Int,
    imgSize : Int,
    noFilt : Int,
    outFormat : ( Int, Int, Int )
  ) : DecoupledIO[Vec[SInt]] = {

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
    val ab = ab_raw.map( _.split(",").toList.map( x => ( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

    val blMod = Module( new SimpleBufferLayer( imgSize, outFormat._3, outFormat, 10, 1, true, tPutLyr ) )
    val conv1 = Module( new TriConvSum( weights_trans, tPutLyr ) )

    val scaleShift = Module( new ScaleAndShift(
      fracBits,
      abFracBits,
      ab(0)take( noFilt ),
      ab(1).take( noFilt ),
      tPutLyr
    ) )

    blMod.io.dataIn <> inputVec
    conv1.io.dataIn <> blMod.io.dataOut
    scaleShift.io.dataIn <> conv1.io.dataOut
    scaleShift.io.dataOut
  }

  def createPoolLyr(
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Int,
    imgSize : Int,
    kernelFormat : ( Int, Int, Int )
  ) : DecoupledIO[Vec[SInt]] = {

    val blMod = Module( new SimpleBufferLayer( imgSize, kernelFormat._3, kernelFormat, 10, 2, false, tPutLyr ) )
    val tPutPool = math.max( tPutLyr / 2, 1 ).toInt // divide by stride
    val poolMod = Module( new PoolLayer( tPutPool, kernelFormat ) )
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
    inputVec : DecoupledIO[Vec[SInt]]
  ) : DecoupledIO[Vec[SInt]] = {
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
    inputVec : DecoupledIO[Vec[SInt]],
    queueSize : Int
  ) : DecoupledIO[Vec[SInt]] = {
    val padQueue = Queue( inputVec, queueSize )
    halveTPut( padQueue )
  }

  def reverseOrder(
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Int
  ) : DecoupledIO[Vec[SInt]] = {
    val noFilt = inputVec.bits.size / tPutLyr
    val bitsGrp = inputVec.bits.toList.grouped( noFilt ).toList.reverse
    val bitsOut = Vec( bitsGrp.reduce( _ ++ _ ) )
    val dcpOut = Wire( Decoupled( bitsOut.cloneType ) )
    dcpOut.valid := inputVec.valid
    dcpOut.bits := bitsOut
    inputVec.ready := dcpOut.ready
    dcpOut
  }

  /*
  val lyr1 = createConvLyr( 1, io.dataIn, tPut, imgSize, 64, ( 3, 3, 3 )  )
  val lyr1Rev = reverseOrder( lyr1, tPut )
  val lyr2 = createConvLyr( 2, lyr1Rev, tPut, imgSize, 64, ( 3, 3, 64 ) )
  val lyr2Rev = reverseOrder( lyr2, tPut )
  val mp1 = createPoolLyr( lyr2Rev, tPut, imgSize, ( 2, 2, 64 ) )
  val mp1Rev = reverseOrder( mp1, tPut / 4 )
  val tPutPart2 = tPut / 4
  val imgSizePart2 = imgSize / 2
   */
  val mp1Rev = io.dataIn
  val tPutPart2 = tPut
  val imgSizePart2 = imgSize
  val lyr3 = createConvLyr( 3, mp1Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 64 )  )
  val lyr3Rev = reverseOrder( lyr3, tPutPart2 )
  val lyr4 = createConvLyr( 4, lyr3Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 128 ) )
  val lyr4Rev = reverseOrder( lyr4, tPutPart2 )
  val mp2 = createPoolLyr( lyr4Rev, tPutPart2, imgSizePart2, ( 2, 2, 128 ) )
  io.dataOut <> mp2
}