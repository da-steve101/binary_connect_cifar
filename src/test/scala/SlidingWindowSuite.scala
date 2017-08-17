
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
      BigInt( myRand.nextInt( 1 << 7 ) ) // ( i * c.grpSize + j ) % ( 1 << 7 ))
    }).toList
  }).toList
  val dataGrped = genData.grouped( c.inSize ).toList
  var outIdx = 0
  var vldCnt = 0
  var inIdx = 0
  var prevGrp = dataGrped( inIdx ).map( x => List.fill( x.size ) { BigInt( 255 ) } )
  while ( inIdx < dataGrped.size ) {
    val newGrp = dataGrped( inIdx )
    val windShift = myRand.nextInt( c.stride )
    val d = prevGrp.takeRight( c.noIgnore ) ++ newGrp.take( c.inSize - c.noIgnore )
    poke( c.io.windShift, windShift )
    val data = d.reduce( _ ++ _ ).toIndexedSeq.reverse
    for ( i <- 0 until data.size )
      poke( c.io.dataIn.bits( i ), data( i ) )
    val vld = ( myRand.nextInt( 5 ) != 0 )
    poke( c.io.dataIn.valid, vld )
    if ( vld ) {
      inIdx += 1
      prevGrp = newGrp
    }
    step( 1 )
    val vldMsk = peek( c.io.vldMsk )
    val vldOut = peek( c.io.dataOut.valid )
    if ( vldOut == 1 ) {
      vldCnt += 1
      for ( idx <- 0 until c.noOut ) {
        if ( vldMsk( c.noOut - 1 - idx ) == 1 ) {
          val offset = ( c.noOut - 1 - idx ) * c.windowSize * c.grpSize
          for ( j <- 0 until c.windowSize * c.grpSize ) {
            val testIdx = outIdx + ( c.padSize - windShift ) * c.grpSize - 1 - j
            if ( testIdx >= 0 )
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
  val paddings = List( false, true )
  backends foreach {backend =>
    it should s"Shift inputs along using $backend" in {
      for ( grpSize <- grpSizes ) {
        for ( inSize <- inSizes ) {
          for ( windowSize <- windowSizes ) {
            for ( stride <- strides ) {
              val bufferOffsets = ( 0 until inSize ).toList
              for ( bufferOffset <- bufferOffsets ) {
                for ( padding <- paddings ) {
                  println( "grpSize = " + grpSize + ", inSize = " + inSize + ", windowSize = " +
                    windowSize + ", stride = " + stride + ", bufferOffset = " + bufferOffset +
                    ", padding = " + padding )
                  val displayBefore = {
                    if ( padding )
                      ( windowSize - 1 )/2
                    else
                      0
                  }
                  Driver(() => {
                    new SlidingWindow( genType, grpSize, inSize, windowSize, stride, bufferOffset, displayBefore )
                  }, backend )( c => new SlidingWindowTests( c ) ) should be (true)
                }
              }
            }
          }
        }
      }
    }
  }
}

