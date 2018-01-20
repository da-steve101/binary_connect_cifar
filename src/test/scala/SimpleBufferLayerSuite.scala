
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.SimpleBufferLayer
import scala.collection.mutable.ArrayBuffer

class SimpleBufferLayerTests( c : SimpleBufferLayer[SInt] ) extends PeekPokeTester( c ) {

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
    // println( "" + imData )
    imData
  })

  val convWin = convSize/2
  val padSize_TL = {
    if ( c.padding )
      0
    else
      ( convSize - 1)/2
  } + ( ( convSize + 1 ) % 2 )
  val padSize_BR = {
    if ( c.padding )
      0
    else
      ( convSize - 1 )/2
  }
  println( "convSize = " + convSize )
  val convOut = ( padSize_TL until c.imgSize - padSize_BR by c.stride ).map( xIdx => {
    ( padSize_TL until c.imgSize - padSize_BR by c.stride ).map( yIdx => {
      ( 0 until convSize ).map( cIdxX => {
        val cx = cIdxX - convWin
        ( 0 until convSize ).map( cIdxY => {
          val cy = cIdxY - convWin
          if ( xIdx + cx >= 0 && xIdx + cx < c.imgSize &&
            yIdx + cy >= 0 && yIdx + cy < c.imgSize )
            img( xIdx + cx )( yIdx + cy ).toList
          else
            List.fill( c.grpSize ) { BigInt( 0 ) } // pad with zeros
        })
      })
    })
  }).reduce( _ ++ _ )

  // println( "convOut = " + convOut )

  val noConvs = convOut.size
  var convCount = 0
  var imgRow = 0
  var imgCol = 0
  var vldSet = false
  println( "noConvs = " + noConvs )
  var rdyCnt = 0
  var vldCnt = 0
  while ( convCount < noImgs * noConvs ) { //  TODO: continue to next img
    /*
    val vld = vldCnt == 0
    vldCnt = ( vldCnt + 1 ) % 4
    val rdy = ( !( peek( c.io.dataOut.valid ) == 1 ) && rdyCnt == 0 ) || rdyCnt == 3 // myRand.nextInt( 4 ) != 0
    if ( peek( c.io.dataOut.valid ) == 1 )
      rdyCnt = ( rdyCnt + 1 ) % 4
     */
    val rdy = ( vldCnt > 8 ) || ( myRand.nextInt( 4 ) != 0 )
    val vld = myRand.nextInt( 2 ) != 0
    if ( vld )
      vldCnt += 1
    if ( rdy && vldCnt > 0 )
      vldCnt -= 1
    poke( c.io.dataOut.ready, rdy )
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
    expect( c.io.dataIn.ready, true ) // assert always ready ...
    val outVld = peek( c.io.dataOut.valid ) == 1
    val outRdy = peek( c.io.dataOut.ready ) == 1
    if ( outRdy && outVld )
      vldSet = false
    if ( outVld && !outRdy )
      vldSet = true
    if ( vldSet )
      expect( c.io.dataOut.valid, vldSet )
    for ( i <- 0 until c.noOut ) {
      if ( outVld ) {
        val offset = i * c.outFormat._1 * c.outFormat._2 * c.outFormat._3
        for ( j <- 0 until outFormat._1 ) {
          for ( k <- 0 until outFormat._2 ) {
            for ( l <- 0 until c.outFormat._3 )
              expect( c.io.dataOut.bits( offset + j*c.outFormat._2*c.outFormat._3 + k*c.outFormat._3 + l ),
                convOut( convCount % noConvs )( c.outFormat._1 - 1 - j )( c.outFormat._2 - 1 - k )( l ) )
          }
        }
        if ( outRdy )
          convCount += 1
      }
    }
    step(1)
  }
}

class SimpleBufferLayerSuite extends ChiselFlatSpec {
  behavior of "SimpleBufferLayer"
  val grpSizes = List( 1, 2, 3, 5, 8 )
  val qSize = 10
  val tPuts = List( 1 ) //, 2, 4, 8 )
  val convImgComb = List( ( 2, 2, 32, false ), ( 3, 1, 32, true ) )
  backends foreach {backend =>
    it should s"buffer inputs on a layer using $backend" in {
      for ( grpSize <- grpSizes ) {
        for ( inputParam <- convImgComb ) {
          for ( tPut <- tPuts ) {
            val imgSize = inputParam._3
            val outFormat = ( inputParam._1, inputParam._1, grpSize )
            val stride = inputParam._2
            val padding = inputParam._4
            println( "imgSize = " + imgSize + ", grpSize = " + grpSize + ", outFormat = " +
              outFormat + ", qSize = " + qSize + ", stride = " + stride + ", padding = " +
              padding + ", tPut = " + tPut )
            Driver(() => {
              new SimpleBufferLayer( SInt( 16.W ), imgSize, grpSize, outFormat, qSize, stride, padding, tPut )
            }, backend, false )( c => new SimpleBufferLayerTests( c ) ) should be (true)
          }
        }
      }
    }
  }
}
