
package binconcifartests

// load the mp_3 output and pass through at 4 tput

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.DenseLayer
import scala.collection.mutable.ArrayBuffer

class DenseComputeTests( c : DenseLayer ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 500

  // val bufferedSource_img = scala.io.Source.fromFile("src/main/resources/airplane4_mp_3.csv")
  // val bufferedSource_out = scala.io.Source.fromFile("src/main/resources/airplane4_fc_1024_preBN.csv")
  val bufferedSource_img = scala.io.Source.fromFile("src/main/resources/airplane4_fc1024.csv" )
  val bufferedSource_out = scala.io.Source.fromFile("src/main/resources/airplane4_softmax_preBN.csv")

  val img_raw = bufferedSource_img.getLines.toList
  /*
  val img = img_raw.map( _.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  }) ).grouped( 4 ).toList
   */
  val img = img_raw.head.split( "," ).toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  })

  val dense_raw = bufferedSource_out.getLines.toList.head
  val dense_res = dense_raw.split(",").toList.map( x => {
    BigInt(( x.toFloat * ( 1 << c.fracBits ) ).toInt)
  })

  var imgRow = 0
  var imgCol = 0
  var imgIdx = 0
  for ( cyc <- 0 until cycs ) {
    val vld = true // myRand.nextInt(4) != 0
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until c.tPut ) {
      // poke( c.io.dataIn.bits( i ), img( imgRow )( imgCol )( imgIdx ) )
      poke( c.io.dataIn.bits( i ), img( imgIdx ) )
      imgIdx += 1
    }
    if ( vld ) {
      if ( imgIdx >= c.noIn ) {
        imgCol += 1
        imgIdx = 0
      }
      if ( imgCol == c.imgSize ) {
        imgCol = 0
        imgRow = ( imgRow + 1 ) % c.imgSize
      }
    } else {
      imgIdx -= c.tPut
    }
    step( 1 )
    expect( c.io.dataIn.ready, true )
    val vldOut = peek( c.io.dataOut.valid ) == 1
    if ( vldOut ) {
      for ( j <- 0 until c.noOut )
        expect( c.io.dataOut.bits( j ), dense_res( j ) )
    }
  }
}

class DenseLayerSuite extends ChiselFlatSpec {

  val bufferedSource_weights = scala.io.Source.fromFile("src/main/resources/softmax_weights.csv")
  val weights_raw = bufferedSource_weights.getLines.toList.tail
  val weights = weights_raw.map( _.split(",").toList.map( x => x.toInt ).toList ).transpose

  println( "weights.size = " + weights.size )
  println( "weights.head.size = " + weights.head.size )

  behavior of "DenseLayer"
  backends foreach {backend =>
    it should s"correctly compute the denseLayer $backend" in {
      Driver(() => {
        new DenseLayer( SInt( 16.W ).cloneType, 1, weights )
      }, "verilator", true )( c => new DenseComputeTests( c ) ) should be (true)
    }
  }
}

