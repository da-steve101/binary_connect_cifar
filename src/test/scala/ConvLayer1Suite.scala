
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import chisel3.util._
import scala.util.Random
import binconcifar.TriConvSum
import binconcifar.SimpleBufferLayer
import binconcifar.ScaleAndShift
import scala.collection.mutable.ArrayBuffer

class ConvLayer1( val throughput : Double ) extends Module {

  val tPut = math.ceil( throughput ).toInt
  val imgSize = 32
  val outFormat = ( 3, 3, 3 )
  val dtype = SInt( 16.W )
  val fracBits = 3
  val abFracBits = 5
  val noOut = 64
  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled( Vec( tPut * 3, dtype.cloneType ) ))
    val dataOut = Decoupled( Vec( tPut * noOut, dtype.cloneType ) )
    val vldMask = Output( Vec( tPut, Bool() ) )
  })

  val bufferedSource = scala.io.Source.fromFile("src/main/resources/conv1_weights.csv")
  val weights_raw = bufferedSource.getLines.toList
  val weights = weights_raw.map( _.split(",").toList.map( x => {
    x.toInt
  }) ).grouped( outFormat._2 ).toList.grouped( outFormat._1 ).toList
  val weights_trans = ( 0 until noOut ).toList.map( i =>
    weights.map( w0 => w0.map( w1 => w1.map( w2 => w2(i) ) ) )
  ).map( x => x.map( _.reverse ).reverse )

  val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/conv1_ab.csv")
  val ab_raw = bufferedSource_ab.getLines.toList
  val ab = ab_raw.map( _.split(",").toList.map( x => ( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

  val blMod = Module( new SimpleBufferLayer( imgSize, outFormat._3, outFormat, 10, 1, true, tPut, true ) )
  // val vldMaskBuff = Module( new VldMaskBuffer( dtype, outFormat._1 * outFormat._2 * outFormat._3, tPut.toInt ) )
  val conv1 = Module( new TriConvSum( weights_trans, throughput ) )
  val scaleShift = Module( new ScaleAndShift( fracBits, abFracBits, ab(0).take( noOut ), ab(1).take( noOut ), tPut ) )

  val latency = 32
  val noIn = tPut

  blMod.io.dataIn <> io.dataIn
  conv1.io.dataIn <> blMod.io.dataOut
  scaleShift.io.dataIn <> conv1.io.dataOut
  io.dataOut <> scaleShift.io.dataOut
  // io.dataOut <> conv1.io.dataOut
}

class ConvLayer1Tests( c : ConvLayer1 ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 800

  val bufferedSource = scala.io.Source.fromFile("src/main/resources/airplane4.csv")
  val img_raw = bufferedSource.getLines.toList
  val img = img_raw.map( _.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  }) ).grouped( c.imgSize ).toList

  val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_conv1_relu.csv")
  // val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane_conv1_act.csv")
  val conv_res_raw = bufferedSource_2.getLines.toList
  val convRes = conv_res_raw.map( _.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  }).take( c.noOut ) ).grouped( c.imgSize ).toList

  var imgRow = 0
  var imgCol = 0
  var convCount = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(10) != 0
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until c.noIn ) {
      for ( j <- 0 until c.outFormat._3 )
        poke( c.io.dataIn.bits( ( c.noIn - 1 - i ) * c.outFormat._3 + j ), img( imgRow )( imgCol )(j) )
      if ( vld && peek( c.io.dataIn.ready ) == 1 ) {
        imgCol += 1
        if ( imgCol == c.imgSize ) {
          imgCol = 0
          imgRow = ( imgRow + 1 ) % c.imgSize
        }
      }
    }
    step( 1 )
    val vldOut = peek( c.io.dataOut.valid ) == 1
    if ( vldOut ) {
      for ( j <- 0 until c.tPut.toInt ) {
        val convX = ( convCount / c.imgSize ) % c.imgSize
        val convY = convCount % c.imgSize
        for ( i <- 0 until c.noOut )
          expect( c.io.dataOut.bits( j * c.noOut + i ), convRes( convX )( convY )(i) )
        convCount += 1
      }
    }
  }
}

class ConvLayer1Suite extends ChiselFlatSpec {

  behavior of "ConvLayer1Suite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => { new ConvLayer1( 0.25 )  }, "verilator", true )( c =>
        new ConvLayer1Tests( c ) ) should be (true)
    }
  }
}
