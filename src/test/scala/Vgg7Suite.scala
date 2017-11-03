
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import chisel3.util._
import scala.util.Random
import binconcifar.Vgg7
import scala.collection.mutable.ArrayBuffer

class Vgg7Tests( c : Vgg7 ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 10000
  val imgOutSize = c.imgOutSize

  val bufferedSource = scala.io.Source.fromFile("src/main/resources/airplane4.csv")
  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/airplane4_conv1_relu.csv")
  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/airplane4_conv2_relu.csv")
  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/airplane4_mp_1.csv")
  val img_raw = bufferedSource.getLines.toList
  val img = img_raw.map( _.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  }) ).grouped( c.imgSize ).toList

  // val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_mp_2.csv")
  val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_mp_1.csv")
  // val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_conv2_relu.csv")
  // val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_conv2_act.csv")
  // val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/airplane4_conv1_relu.csv")
  val conv_res_raw = bufferedSource_2.getLines.toList
  val convRes = conv_res_raw.map( _.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  }).take( c.noOut ) ).grouped( imgOutSize ).toList

  var imgRow = 0
  var imgCol = 0
  var convCount = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(4) != 0
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until c.noIn ) {
      for ( j <- 0 until c.inGrp )
        poke( c.io.dataIn.bits( ( c.noIn - 1 - i ) * c.inGrp + j ), img( imgRow )( imgCol )(j) )
      if ( vld ) {
        imgCol += 1
        if ( imgCol == c.imgSize ) {
          imgCol = 0
          imgRow = ( imgRow + 1 ) % c.imgSize
        }
      }
    }
    step( 1 )
    expect( c.io.dataIn.ready, true )
    val vldOut = peek( c.io.dataOut.valid ) == 1
    if ( vldOut ) {
      for ( j <- 0 until c.tPutOut.toInt ) {
        val convX = ( convCount / imgOutSize ) % imgOutSize
        val convY = convCount % imgOutSize
        for ( i <- 0 until c.noOut )
          expect( c.io.dataOut.bits( j * c.noOut + i ), convRes( convX )( convY )(i) )
        convCount += 1
      }
    }
  }
}

class Vgg7Suite extends ChiselFlatSpec {

  behavior of "Vgg7Suite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => { new Vgg7  }, "verilator", false )( c =>
        new Vgg7Tests( c ) ) should be (true)
    }
  }
}
