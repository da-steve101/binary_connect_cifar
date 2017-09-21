
package binconcifar

import chisel3._
import chisel3.util._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class Vgg7 extends Module {

  val tPut = 1
  val tPutOut = 1
  val imgSize = 32
  val dtype = SInt( 16.W )
  val fracBits = 3
  val abFracBits = 5
  val inGrp = 64
  val noOut = 64
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
    outFormat : ( Int, Int, Int )
  ) : DecoupledIO[Vec[SInt]] = {

    val bufferedSource = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_weights.csv" )
    val weights_raw = bufferedSource.getLines.toList
    val weights = weights_raw.map( _.split(",").toList.map( x => {
      x.toInt
    }) ).grouped( outFormat._2 ).toList.grouped( outFormat._1 ).toList
    val weights_trans = ( 0 until noOut ).toList.map( i =>
      weights.map( w0 => w0.map( w1 => w1.map( w2 => w2(i) ) ) )
    ).map( x => x.map( _.reverse ).reverse )

    val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_ab.csv")
    val ab_raw = bufferedSource_ab.getLines.toList
    val ab = ab_raw.map( _.split(",").toList.map( x => ( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

    val blMod = Module( new BufferLayer( imgSize, outFormat._3, outFormat, 10, 1, true, tPutLyr ) )
    val conv1 = Module( new TriConvSum( weights_trans, tPutLyr ) )
    val scaleShift = Module( new ScaleAndShift(
      fracBits,
      abFracBits,
      ab(0).take( noOut ),
      ab(1).take( noOut ),
      tPutLyr
    ) )

    blMod.io.dataIn <> inputVec
    println( blMod.io.dataOut.bits.size )
    println( conv1.io.dataIn.bits.size )
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

    val blMod = Module( new BufferLayer( imgSize, kernelFormat._3, kernelFormat, 10, 2, false, tPutLyr ) )
    val tPutPool = math.max( tPutLyr / 4, 1 ).toInt // divide by stride
    val poolMod = Module( new PoolLayer( tPutPool, kernelFormat ) )
    blMod.io.dataIn <> inputVec
    poolMod.io.dataIn <> blMod.io.dataOut
    poolMod.io.dataOut
  }

  // val lyr1 = createConvLyr( 1, io.dataIn, tPut, imgSize, ( 3, 3, inGrp )  )
  val lyr2 = createConvLyr( 2, io.dataIn, tPut, imgSize, ( 3, 3, 64 ) )
  // val mp1 = createPoolLyr( io.dataIn, tPut, imgSize, ( 2, 2, 64 ) )
  io.dataOut <> lyr2
}
