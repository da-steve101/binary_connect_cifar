
package binconcifartests

// load the mp_3 output and pass through at 4 tput

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.DenseScale
import scala.collection.mutable.ArrayBuffer

class DenseScaleTests( c : DenseScale ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 5000

  val bufferedSource_img = scala.io.Source.fromFile("src/main/resources/airplane4_fc_1024_preBN.csv")
  val bufferedSource_out = scala.io.Source.fromFile("src/main/resources/airplane4_fc1024.csv" )

  val img_raw = bufferedSource_img.getLines.toList

  val img = img_raw.head.split( "," ).toList.map( x => {
    BigInt(math.round( x.toFloat * ( 1 << c.convPrec ) ).toInt)
  })

  val dense_raw = bufferedSource_out.getLines.toList.head
  val dense_res = dense_raw.split(",").toList.map( x => {
    BigInt(math.round( x.toFloat * ( 1 << c.convPrec ) ).toInt)
  })

  var imgIdx = 0
  var outIdx = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(4) != 0
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until 1 ) {
      poke( c.io.dataIn.bits( i ), img( imgIdx ) )
      imgIdx += 1
    }
    if ( vld ) {
      if ( imgIdx >= 1024 )
        imgIdx = 0
    }
    else
      imgIdx -= 1
   
    step( 1 )
    expect( c.io.dataIn.ready, true )
    val vldOut = peek( c.io.dataOut.valid ) == 1

    if ( vldOut ) {
      expect( c.io.dataOut.bits( 0 ), dense_res( outIdx ) )
      outIdx += 1
    } else
      peek( c.io.dataOut.bits( 0 ) )
  }
}

class DenseScaleSuite extends ChiselFlatSpec {

  val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/fc_1024_ab.csv")
  val ab_raw = bufferedSource_ab.getLines.toList
  val ab = ab_raw.map( _.split(",").toList.map( x => math.round( x.toFloat * ( 1 << 6 ) ).toInt ) )

  behavior of "DenseScale"
  backends foreach {backend =>
    it should s"correctly compute the denseScale $backend" in {
      Driver(() => {
        new DenseScale( SInt( 16.W ).cloneType, ab(0), ab(1), 4, 6 )
      }, "verilator", true )( c => new DenseScaleTests( c ) ) should be (true)
    }
  }
}

