
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.SlidingWindow
import scala.collection.mutable.ArrayBuffer

class SlidingWindowTests( c : SlidingWindow[UInt] ) extends PeekPokeTester( c ) {
  val cycs = 5*c.windowSize
  val myRand = new Random
  val genData = ( 0 until c.inSize * cycs ).map( i => {
    ( 0 until c.grpSize ).map( j => {
      BigInt( ( i*c.grpSize + j ) % ( 1 << 7 ) )
    }).toList
  }).toList
  println( "genData = " + genData )
  var outIdx = 0
  var vldCnt = 0
  for ( d <- genData.grouped( c.inSize ) ) {
    val data = d.reduce( _ ++ _ ).toIndexedSeq.reverse
    println( "data = " + data )
    for ( i <- 0 until data.size )
      poke( c.io.dataIn.bits( i ), data( i ) )
    val vld = true
    poke( c.io.dataIn.valid, vld )
    step( 1 )
    println( "dataOut = " + peek( c.io.dataOut.bits ) )
    val vldMsk = peek( c.io.vldMsk )
    println( "vldMsk = " + vldMsk )
    if ( peek( c.io.dataOut.valid ) == 1 ) {
      println( "vldCnt = " + vldCnt )
      vldCnt += 1
      for ( idx <- 0 until c.noOut ) {
        if ( vldMsk( c.noOut - 1 - idx ) == 1 ) {
          val offset = ( c.noOut - 1 - idx ) * c.windowSize * c.grpSize
          for ( j <- 0 until c.windowSize * c.grpSize ) {
            val testIdx = outIdx + c.windowSize * c.grpSize - 1 - j
            expect( c.io.dataOut.bits( offset + j ), genData( testIdx / c.grpSize )( testIdx % c.grpSize ) )
          }
          outIdx += c.stride * c.grpSize
        }
      }
    }
  }
  assert( vldCnt > 0, "Should have valid data" )
}


class SlidingWindowSuite extends ChiselFlatSpec {
  behavior of "SlidingWindow"
  val genType = 0.U( 8.W )
  val grpSizes = List( 1, 2, 3 )
  val inSizes = List( 1, 2, 3, 4 )
  val windowSizes = List( 3, 4, 5, 7, 10 )
  val strides = List( 1, 2, 3, 4 )
  backends foreach {backend =>
    it should s"Shift inputs along using $backend" in {
      for ( grpSize <- grpSizes ) {
        for ( inSize <- inSizes ) {
          for ( windowSize <- windowSizes ) {
            for ( stride <- strides ) {
              println( "grpSize = " + grpSize + ", inSize = " + inSize +
                ", windowSize = " + windowSize + ", stride = " + stride )
              Driver(() => {
                new SlidingWindow( genType, grpSize, inSize, windowSize, stride )
              }, backend )( c => new SlidingWindowTests( c ) ) should be (true)
            }
          }
        }
      }
    }
  }
}
