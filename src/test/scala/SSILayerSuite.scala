package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.SSILayer
import scala.collection.mutable.ArrayBuffer

class SSILayerTests[T <: Bits]( c : SSILayer[T] ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val testImg = ( 0 until c.bufLen*16 ).map( i => {
    BigInt( i % ( 1 << 16 ) )
  }).toList
  val noCyc = c.bufLen*16 / c.tPutIn

  var cyc : Int = 0
  var outIdx : Int = 0
  var stallCycIn : Int = 0
  var stallCycOut : Int = 0
  var offsetOut : Int = 0
  while ( cyc < noCyc ) {
    val vldMask = myRand.nextInt( 4 ) != 0
    val partCyc = cyc % scala.math.max( c.ratioOut, 1 )
    if ( c.tPutOut > c.tPutIn && partCyc == 0 && !vldMask )
      stallCycOut = 1
    else
      stallCycOut = 0
    poke( c.io.dataIn.valid, stallCycIn == 0 && stallCycOut == 0 )
    if ( c.tPutOut > c.tPutIn ) {
      val wholeCyc = ( cyc - partCyc )/c.ratioOut
      for ( i <- 0 until c.tPutIn )
        poke( c.io.dataIn.bits( i ), testImg( ( wholeCyc + 1)*c.tPutOut - ( partCyc + 1 )*c.tPutIn + i ) )
    } else {
      for ( i <- 0 until c.tPutIn )
        poke( c.io.dataIn.bits( i ), testImg( i + cyc*c.tPutIn ) )
    }
    if ( stallCycIn == 0 && stallCycOut == 0 ) {
      if ( c.tPutIn >= c.tPutOut )
        stallCycIn = c.ratioIn
      cyc = cyc + 1
    }
    if ( stallCycIn > 1 || stallCycIn == 1 & vldMask )
      stallCycIn = stallCycIn - 1
    val vldOut = peek( c.io.dataOut.valid ) == 1
    // peek( c.io.dataOut.bits )
    if ( vldOut ) {
      for ( j <- 0 until c.tPutOut )
        expect( c.io.dataOut.bits( j ), testImg( ( outIdx + 1 )*c.bufLen - (offsetOut + 1)*c.tPutOut + j ) )
      if ( offsetOut + 1 >= c.ratioIn ) {
        offsetOut = 0
        outIdx = outIdx + 1
      } else
          offsetOut = offsetOut + 1
    }
    step( 1 )
  }
}

class SSILayerSuite extends ChiselFlatSpec {
  behavior of "SSILayer"
  val tPutPairs = List( ( 256, 256 ), ( 256, 64 ), ( 256, 16), (256,4), ( 64, 256 ), ( 16, 256 ), ( 4, 256 ) )
  for ( pair <- tPutPairs ) {
    println( "pair = " + pair)
    Driver(() => {
      new SSILayer( UInt( 16.W ), pair._1, pair._2 )
    }, "verilator", true )( c => new SSILayerTests( c ) )
  }
}

