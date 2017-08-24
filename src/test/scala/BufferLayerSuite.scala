
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.BufferLayer
import scala.collection.mutable.ArrayBuffer

class BufferLayerTests( c : BufferLayer ) extends PeekPokeTester( c ) {

  val convSize = c.outFormat._2
  val outFormat = c.outFormat

  val myRand = new Random
  val noImgs = 1

  println( "img = " )

  val img = ( 0 until c.imgSize ).map( i => {
    val imData = ( 0 until c.imgSize ).map( j => {
      ( 0 until outFormat._3 ).map( k => {
        val tmp = i*c.imgSize*outFormat._3 + j*outFormat._3 + k
        BigInt( tmp % ( 1 << 7 ) )
      })
    })
    println( "" + imData )
    imData
  })

  val convWin = (convSize - 1)/2
  val padSize = {
    if ( c.padding )
      0
    else
      convWin
  }

  val convOut = ( padSize until c.imgSize - padSize by c.stride ).map( xIdx => {
    ( padSize until c.imgSize - padSize by c.stride ).map( yIdx => {
      ( -convWin to convWin ).map( cx => {
        ( -convWin to convWin ).map( cy => {
          if ( xIdx + cx >= 0 && xIdx + cx < c.imgSize &&
            yIdx + cy >= 0 && yIdx + cy < c.imgSize )
            img( xIdx + cx )( yIdx + cy ).toList
          else
            List.fill( c.grpSize ) { BigInt( 0 ) } // pad with zeros
        })
      })
    })
  }).reduce( _ ++ _ )

  println( "convOut = " + convOut )

  val noConvs = convOut.size
  var convCount = 0
  var imgRow = 0
  var imgCol = 0
  println( "noConvs = " + noConvs )
  while ( convCount < noImgs * noConvs ) { //  TODO: continue to next img
    val vld = ( peek( c.io.dataIn.ready ) == 1 ) && ( myRand.nextInt( 5 ) != 0 )
    println( "vld = " + vld )
    poke( c.io.dataOut.ready, true )
    poke( c.io.dataIn.valid, vld )
    for ( i <- 0 until c.noIn ) {
      for ( j <- 0 until c.outFormat._3 )
        poke( c.io.dataIn.bits( ( c.noIn - 1 - i ) * c.outFormat._3 + j ), img( imgRow )( imgCol )(j) )
      if ( vld ) {
        imgCol += 1
        if ( imgCol == c.imgSize ) {
          imgCol = 0
          imgRow = ( imgRow + 1 ) % c.imgSize
        }
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
        if ( vldMsk ) {
          val offset = i * c.outFormat._1 * c.outFormat._2 * c.outFormat._3
          for ( j <- 0 until outFormat._1 ) {
            for ( k <- 0 until outFormat._2 ) {
              for ( l <- 0 until c.outFormat._3 )
                expect( c.io.dataOut.bits( offset + j*c.outFormat._2*c.outFormat._3 + k*c.outFormat._3 + l ),
                  convOut( convCount % noConvs )( c.outFormat._1 - 1 - j )( c.outFormat._2 - 1 - k )( l ) )
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
  val grpSizes = List( 1, 2, 3, 5, 8 )
  val qSize = 10
  val strides = List( 1, 2, 3, 4 )
  val paddings = List( false, true )
  val tPuts = List( 1, 2 )
  val convImgComb = List( ( 3, 5 ), ( 5, 9 ),  ( 3, 8 ), ( 5, 8 ) )
  backends foreach {backend =>
    it should s"buffer inputs on a layer using $backend" in {
      for ( grpSize <- grpSizes ) {
        for ( stride <- strides ) {
          for ( padding <- paddings ) {
            for ( tPut <- tPuts ) {
              for ( inputParam <- convImgComb ) {
                val imgSize = inputParam._2
                val outFormat = ( inputParam._1, inputParam._1, grpSize )
                println( "imgSize = " + imgSize + ", grpSize = " + grpSize + ", outFormat = " +
                  outFormat + ", qSize = " + qSize + ", stride = " + stride + ", padding = " +
                  padding + ", tPut = " + tPut )
                Driver(() => {
                  new BufferLayer( imgSize, grpSize, outFormat, qSize, stride, padding, tPut )
                }, backend )( c => new BufferLayerTests( c ) ) should be (true)
              }
            }
          }
        }
      }
    }
  }
}
