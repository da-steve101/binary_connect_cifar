
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.Im2Col
import scala.collection.mutable.ArrayBuffer

class Im2ColTests[T <: Bits]( c : Im2Col[T] ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val testImg = ( 0 until c.imgSize * c.imgSize * c.grpSize ).map( i => {
    BigInt( i % ( 1 << 7 ) )
  }).toList
  val noConvs = 2 * c.imgSize * c.imgSize / ( c.stride * c.stride )
  var convCounts = 0
  var imgIdx = 0
  val padAmt = {
    if ( c.padding )
      - ( c.kernelSize / 2 ).toInt
    else
      0
  }
  var cyc = 0
  var convX = padAmt
  var convY = padAmt
  var inCyc = 0
  var stallCyc = myRand.nextInt( 100 )
  print( "stallCyc = " + stallCyc )
  val invTPut = ( 1 / c.tPut ).toInt
  while ( convCounts < noConvs ) {
    if ( inCyc % invTPut == 0 )
      poke( c.io.dataIn.valid, stallCyc == 0 )
    else
      poke( c.io.dataIn.valid, false )
    for ( i <- 0 until c.grpSize ) {
      val testPix = testImg( ( imgIdx * c.grpSize + i ) % ( c.imgSize * c.imgSize ) )
      val testValue = ( testPix >> ( c.dWidth * ( inCyc / c.inputCycles ).toInt ) ) % ( 1 << c.dWidth )
      poke( c.io.dataIn.bits( i ), testValue )
    }
    if ( stallCyc > 0 )
      stallCyc = stallCyc - 1
    else {
      inCyc = ( inCyc + 1 ) % ( c.inputCycles * invTPut )
      if ( inCyc == 0 ) {
        imgIdx = imgIdx + 1
        if ( imgIdx % ( c.imgSize * c.imgSize ) == 0 ) {
          stallCyc = myRand.nextInt(100)
          print( "stallCyc = " + stallCyc )
        }
      }
    }
    val vldOut = peek( c.io.dataOut.valid ) == 1
    peek( c.io.dataOut.bits )
    if ( vldOut ) {
      for ( i <- 0 until c.kernelSize ) {
        for ( j <- 0 until c.kernelSize ) {
          val convYi = convY + c.kernelSize - i - 1
          val convXj = convX + c.kernelSize - j - 1
          if ( c.padding && ( convYi < 0  || convXj < 0 || convYi >= c.imgSize || convXj >= c.imgSize ) ) {
            for ( k <- 0 until c.grpSize )
              expect( c.io.dataOut.bits( ( i * c.kernelSize + j )*c.grpSize + k ), BigInt( 0 ) )
          } else {
            val idx = convYi * c.imgSize + convXj
            for ( k <- 0 until c.grpSize ) {
              val testValue = ( testImg( idx * c.grpSize + k ) >> ( cyc * c.outWidth ) ) % BigInt( 1 << c.outWidth )
              expect( c.io.dataOut.bits( ( i * c.kernelSize + j )*c.grpSize + k ), testValue )
            }
          }
        }
      }
      cyc += 1
      if ( cyc >= ( c.inputCycles / c.tPut ).toInt ) {
        convX += c.stride
        cyc = 0
      }
      if ( convX >= c.imgSize - 1 ) {
        convX = padAmt
        convY += c.stride
      }
      if ( convY >= c.imgSize - 1 )
        convY = padAmt
      convCounts += 1
    }
    step( 1 )
  }
}

class Im2ColSuite extends ChiselFlatSpec {
  behavior of "Im2Col"
  val grpSizes = List( 3 )//, 2, 3, 5, 8 )
  val qSize = 10
  val tPuts = List( 0.25 )
  val inputCycles = List( 1 )
  val convImgComb = List( ( 2, 2, 32, false, true ) )// ,*/ ( 3, 1, 32, true, false ) ) //, ( 3, 1, 32, true, true ) )

  for ( grpSize <- grpSizes ) {
    for ( inputParam <- convImgComb ) {
      for ( inputCycle <- inputCycles ) {
        for ( tPut <- tPuts ) {
          val imgSize = inputParam._3
          val kernelSize = inputParam._1
          val stride = inputParam._2
          val padding = inputParam._4
          val noFifo = inputParam._5
          val dWidth = ( 16/inputCycle ).toInt
          println( "imgSize = " + imgSize + ", grpSize = " + grpSize + ", kernelSize = " +
            kernelSize + ", qSize = " + qSize + ", stride = " + stride + ", padding = " +
            padding + ", tPut = " + tPut + ", noFifo = " + noFifo )
          Driver(() => {
            new Im2Col( UInt( dWidth.W ), imgSize, grpSize, kernelSize, qSize, stride, padding, tPut, inputCycle, noFifo = noFifo )
          }, "verilator", true )( c => new Im2ColTests( c ) )
        }
      }
    }
  }
}


