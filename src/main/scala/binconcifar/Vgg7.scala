
package binconcifar

import chisel3._
import chisel3.util._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Vgg7( dtype : SInt ) extends Module {

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
    val dataIn : DecoupledIO[Vec[SInt]] = Flipped(Decoupled( Vec( tPutPart1Int * inGrp, dtype ) ))
    val dataOut : DecoupledIO[Vec[SInt]] = Decoupled( Vec( tPutOut * noOut, dtype.cloneType ) )
  })

  def createSparseMulLyr(
    idx : Int,
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Double,
    imgSize : Int,
    noFilt : Int,
    grpSize : Int,
    kernelSize : Int,
    fanoutReg : Int,
    prefix : String = "conv",
    noFifo : Boolean = false,
    bfr_reg : Int = 0
  ) : DecoupledIO[Vec[SInt]] = {

    val bufferedSource = scala.io.Source.fromFile("src/main/resources/" + prefix + idx + "_tern_op_list.csv" )
    val data_src = bufferedSource.getLines.toList
    val data_ints = data_src.map( _.split(",").toList.map( x => {
      x.toInt
    }))

    val outputIdxs = data_ints.head.toList
    val treeDefinition = data_ints.tail.toList

    val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_ab.csv")
    val ab_raw = bufferedSource_ab.getLines.toList
    val ab = ab_raw.map( _.split(",").toList.map( x => math.round( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

    val tPutInt = math.max( tPutLyr, 1 ).toInt
    val blMod = Module( new Im2Col( dtype, imgSize, grpSize, kernelSize, 10, 1, true, tPutLyr, 1, noFifo = noFifo ) )

    val scaleShift = Module( new ScaleAndShift(
      dtype,
      fracBits,
      abFracBits,
      ab(0)take( noFilt ),
      ab(1).take( noFilt ),
      tPutInt
    ) )

    blMod.io.dataIn.bits := inputVec.bits
    blMod.io.dataIn.valid := inputVec.valid
    inputVec.ready := true.B
    val modOrder = Vec( blMod.io.dataOut.bits.grouped(grpSize).toList.reverse.reduce( _ ++ _ ) )
    if ( tPutLyr >= 1 ){
      val smm = Module( new SparseMatMul( dtype, treeDefinition, outputIdxs ) )
      smm.io.dataIn.bits := Vec( modOrder.map( _.toSInt ) )
      smm.io.dataIn.valid := blMod.io.dataOut.valid
      scaleShift.io.dataIn.bits := ShiftRegister( smm.io.dataOut.bits, bfr_reg )
      scaleShift.io.dataIn.valid := ShiftRegister( smm.io.dataOut.valid, bfr_reg, false.B, true.B )
    } else {
      val smms = Module( new SparseMatMulSerial( dtype, treeDefinition, outputIdxs, (16 * tPutLyr).toInt, fanoutReg ) )
      smms.io.dataIn.bits := modOrder
      smms.io.dataIn.valid := blMod.io.dataOut.valid
      scaleShift.io.dataIn.bits := ShiftRegister( smms.io.dataOut.bits, bfr_reg )
      scaleShift.io.dataIn.valid := ShiftRegister( smms.io.dataOut.valid, bfr_reg, false.B, true.B )
    }

    // blMod.io.dataOut.ready := conv1.io.dataIn.ready
    scaleShift.io.dataOut
  }

  def createConvLyr(
    idx : Int,
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Double,
    imgSize : Int,
    noFilt : Int,
    outFormat : ( Int, Int, Int ),
    fanoutReg : Int,
    noFifo : Boolean = false
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
    val ab = ab_raw.map( _.split(",").toList.map( x => math.round( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

    val tPutInt = math.max( tPutLyr, 1 ).toInt
    val blMod = Module( new SimpleBufferLayer( dtype, imgSize, outFormat._3, outFormat, 10, 1, true, tPutInt, noFifo = noFifo ) )
    val conv1 = Module( new TriConvSum( dtype, weights_trans, tPutLyr, fanoutReg ) )

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
    inputVec : DecoupledIO[Vec[SInt]],
    tPutLyr : Int,
    imgSize : Int,
    kernelFormat : ( Int, Int, Int )
  ) : DecoupledIO[Vec[SInt]] = {

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

  def SSIChange(
    inputVec : DecoupledIO[Vec[SInt]],
    noIn : Int,
    noDown : Int
  ) : DecoupledIO[Vec[SInt]] = {
    val SSIDown = Module( new SSILayer( dtype, noIn, noDown ) )
    SSIDown.io.dataIn <> inputVec
    val SSIUp = Module( new SSILayer( dtype, noDown, noIn ) )
    SSIUp.io.dataIn <> SSIDown.io.dataOut
    SSIUp.io.dataOut
  }

  // val lyr1 = createConvLyr( 1, io.dataIn, tPut, imgSize, 64, ( 3, 3, 3 ), 0, true )
  val lyr1 = createSparseMulLyr( 1, io.dataIn, tPut, imgSize, 64, 3, 3, 0, "conv", true )
  val lyr1Rev = reverseOrder( lyr1, tPutPart1Int )
  // val lyr2 = createConvLyr( 2, lyr1Rev, tPut, imgSize, 64, ( 3, 3, 64 ), 0, true )
  val lyr2 = createSparseMulLyr( 2, lyr1Rev, tPut, imgSize, 64, 64, 3, 0, "conv", true )
  val lyr2Rev = reverseOrder( lyr2, tPutPart1Int )
  val mp1 = createPoolLyr( lyr2Rev, tPutPart1Int, imgSize, ( 2, 2, 64 ) )
  val ssi1 = SSIChange( mp1, 64, 32 ) // can only do 2 without another buffer

  // io.dataOut <> ssi1

  val tPutPart2 = tPut / 4
  val tPutPart2Int = math.max( tPutPart2, 1 ).toInt
  val mp1Rev = reverseOrder( ssi1, tPutPart2Int )
  val imgSizePart2 = imgSize / 2

  // val lyr3 = createConvLyr( 3, mp1Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 64 ), 2 )
  val lyr3 = createSparseMulLyr( 3, mp1Rev, tPutPart2, imgSizePart2, 128, 64, 3, 2 )
  val lyr3Rev = reverseOrder( lyr3, tPutPart2Int )
  // val lyr4 = createConvLyr( 4, lyr3Rev, tPutPart2, imgSizePart2, 128, ( 3, 3, 128 ), 2 )
  val lyr4 = createSparseMulLyr( 4, lyr3Rev, tPutPart2, imgSizePart2, 128, 128, 3, 2, noFifo = true, bfr_reg = 2 )
  val lyr4Rev = reverseOrder( lyr4, tPutPart2Int )
  val mp2 = createPoolLyr( lyr4Rev, tPutPart2Int, imgSizePart2, ( 2, 2, 128 ) )
  val ssi2 = SSIChange( mp2, 128, 16 ) // can only do 2 without another buffer

  // io.dataOut <> ssi2

  val tPutPart3 = tPutPart2 / 4
  val tPutPart3Int = math.max( tPutPart3, 1 ).toInt
  val mp2Rev = reverseOrder( ssi2, tPutPart3Int )
  val imgSizePart3 = imgSize / 4

  // val lyr5 = createConvLyr( 5, mp2Rev, tPutPart3, imgSizePart3, 256, ( 3, 3, 128 ), 2  )
  val lyr5 = createSparseMulLyr( 5, mp2Rev, tPutPart3, imgSizePart3, 256, 128, 3, 2, bfr_reg = 2  )
  val lyr5Rev = reverseOrder( lyr5, tPutPart3Int )
  // val lyr6 = createConvLyr( 6, lyr5Rev, tPutPart3, imgSizePart3, 256, ( 3, 3, 256 ), 2 )
  val ssi3 = SSIChange( lyr5Rev, 256, 16 ) // can only do 2 without another buffer
  val lyr6 = createSparseMulLyr( 6, ssi3, tPutPart3, imgSizePart3, 256, 256, 3, 2, noFifo = true, bfr_reg = 2 )
  val lyr6Rev = reverseOrder( lyr6, tPutPart3Int )
  val mp3 = createPoolLyr( lyr6Rev, tPutPart3Int, imgSizePart3, ( 2, 2, 256 ) )

  io.dataOut <> mp3

}
