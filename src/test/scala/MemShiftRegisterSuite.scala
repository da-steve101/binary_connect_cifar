
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.MemShiftRegister
import scala.collection.mutable.ArrayBuffer

class MemShiftRegisterTests[T <: Bits]( c : MemShiftRegister[Vec[T]], bitWidth : Int ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 1000

  val tmpsr = ArrayBuffer[List[BigInt]]()
  for ( i <- 0 until cycs ) {
    tmpsr += List.fill( c.io.in.size ) {
      val x = myRand.nextInt( 1 << bitWidth )
      BigInt(x)
    }
  }

  var inIdx = 0
  var outIdx = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(4) != 0
    poke( c.io.en, vld )
    for ( i <- 0 until c.io.in.size ) {
      if ( vld )
        poke( c.io.in( i ), tmpsr( inIdx )( i ) )
      else
        poke( c.io.in( i ), BigInt( 0 ) )
    }
    if ( vld )
      inIdx += 1

    step( 1 )
    if ( vld && inIdx >= c.n ) {
      for ( i <- 0 until c.io.in.size ) {
        if ( c.io.in.head.isInstanceOf[SInt] && tmpsr( outIdx )( i ) >= ( 1 << ( bitWidth - 1 ) ) )
          expect( c.io.out( i ), tmpsr( outIdx )(i) - BigInt( 1 <<  bitWidth ) )
        else
          expect( c.io.out( i ), tmpsr( outIdx )(i) )
      }
      outIdx += 1
    }
  }
}

class MemShiftRegisterSuite extends ChiselFlatSpec {

  val bitWidth = 4
  val dataType = SInt( bitWidth.W )
  behavior of "MemShiftRegister"
  backends foreach {backend =>
    it should s"correctly compute the memshiftregister $backend" in {
      Driver(() => {
        new MemShiftRegister( Vec( 64, dataType ), 32 )
      }, "verilator", true )( c => new MemShiftRegisterTests( c, bitWidth ) ) should be (true)
    }
  }
}

