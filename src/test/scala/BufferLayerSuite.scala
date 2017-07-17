
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.BufferLayer
import scala.collection.mutable.ArrayBuffer

class BufferLayerTests( c : BufferLayer ) extends PeekPokeTester( c ) {

  val stride = 1
  val padding = false
  val imgSize = c.imgSize
  val convSize = c.outFormat._2
  val outFormat = c.outFormat
  val tPut = 1

  val myRand = new Random

  println( "img = " )
  /*
  val img = List.fill( imgSize ) {
    val imData = List.fill( imgSize ) {
      List.fill( outFormat._3 ) {
        BigInt( myRand.nextInt( 1 << 6 ) )
      }
    }
    println( "" + imData )
    imData
  }
   */

  val img = ( 0 until imgSize ).map( i => {
    val imData = ( 0 until imgSize ).map( j => {
      ( 0 until outFormat._3 ).map( k => {
        val tmp = i*imgSize*outFormat._3 + j*outFormat._3 + k
        BigInt( tmp % ( 1 << 6 ) )
      })
    })
    println( "" + imData )
    imData
  })

  val padSize = (convSize - 1)/2

  val convOut = ( padSize until imgSize - padSize by stride ).map( xIdx => {
    ( padSize until imgSize - padSize by stride ).map( yIdx => {
      ( -padSize to padSize ).map( cx => {
        ( -padSize to padSize ).map( cy => {
          img( xIdx + cx )( yIdx + cy )
        }).reduce( _ ++ _ )
      }).reduce( _ ++ _ )
    })
  }).reduce( _ ++ _ )

  println( "convOut = " + convOut )

  val noConvs = math.pow( imgSize + 1 - convSize, 2 )
  var convCount = 0
  var imgRow = 0
  var imgCol = 0
  println( "noConvs = " + noConvs )
  while ( convCount < noConvs ) {
    val vld = peek( c.io.dataIn.ready ) == 1
    println( "vld = " + vld )
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until c.throughput.toInt ) {
      for ( j <- 0 until c.outFormat._3 )
        poke( c.io.dataIn.bits( i*c.outFormat._3 + j ), img( imgRow )( imgCol )(j) )
      imgCol += 1
      if ( imgCol == imgSize ) {
        imgCol = 0
        imgRow = ( imgRow + 1 ) % imgSize
      }
    }
    step(1)
    val outVld = peek( c.io.dataOut.valid ) == 1
    println( "bits = " + peek( c.io.dataOut.bits ) )
    println( "outVld = " + outVld )
    if ( outVld ) {
      for ( j <- 0 until convOut( convCount ).size )
        expect( c.io.dataOut.bits(j), convOut( convCount )(j) )
      convCount += 1
    }
  }
}

class BufferLayerSuite extends ChiselFlatSpec {
  behavior of "BufferLayer"
  val inSize = 3
  val qSize = 10
  val stride = 1
  val padding = false
  backends foreach {backend =>
    it should s"buffer inputs on a layer $backend" in {
      for ( tPut <- List( 1, 2 ) ) {
        for ( inputParam <- List( 3, 5 ).zip( List( 5, 32 ) ) ) {
          val imgSize = inputParam._2
          val outFormat = ( inputParam._1, inputParam._1, inSize )
          Driver(() => {
            new BufferLayer( imgSize, inSize, outFormat, qSize, stride, padding, tPut )
          }, backend )( c => new BufferLayerTests( c ) ) should be (true)
        }
      }
    }
  }
}
