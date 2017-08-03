
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

  val img = ( 0 until imgSize ).map( i => {
    val imData = ( 0 until imgSize ).map( j => {
      ( 0 until outFormat._3 ).map( k => {
        val tmp = i*imgSize*outFormat._3 + j*outFormat._3 + k
        BigInt( tmp % ( 1 << 7 ) )
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
        })
      })
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
    for ( i <- 0 until c.noOut ) {
      for ( j <- 0 until c.outFormat._3 )
        poke( c.io.dataIn.bits( ( c.noOut - 1 - i ) * c.outFormat._3 + j ), img( imgRow )( imgCol )(j) )
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
    for ( i <- 0 until c.noOut ) {
      val vldMsk = peek( c.io.vldMask( i ) ) == 1
      println( "vldMsk( " + i + " ) = " + vldMsk )
      if ( outVld ) {
        if ( vldMsk && convCount < noConvs ) {
          val offset = i * c.outFormat._1 * c.outFormat._2 * c.outFormat._3
          for ( j <- 0 until outFormat._1 ) {
            for ( k <- 0 until outFormat._2 ) {
              for ( l <- 0 until c.outFormat._3 )
                expect( c.io.dataOut.bits( offset + j*c.outFormat._2*c.outFormat._3 + k*c.outFormat._3 + l ),
                  convOut( convCount )( c.outFormat._1 - 1 - j )( c.outFormat._2 - 1 - k )( l ) )
            }
          }
          convCount += 1
        }
      }
    }
  }
}

class BufferLayerSuite extends ChiselFlatSpec {
  behavior of "BufferLayer"
  val inSize = 1
  val qSize = 10
  val stride = 1
  val padding = false
  backends foreach {backend =>
    for ( tPut <- List( 2 ) ) {
      for ( inputParam <- List( 3 ).zip( List( 5 ) ) ) {
        val imgSize = inputParam._2
        val outFormat = ( inputParam._1, inputParam._1, inSize )
        it should s"buffer inputs on a layer with tPut = $tPut and $inputParam using $backend" in {
          Driver(() => {
            new BufferLayer( imgSize, inSize, outFormat, qSize, stride, padding, tPut )
          }, backend )( c => new BufferLayerTests( c ) ) should be (true)
        }
      }
    }
  }
}
