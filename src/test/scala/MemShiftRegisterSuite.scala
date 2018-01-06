
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.MemShiftRegister
import scala.collection.mutable.ArrayBuffer

class MemShiftRegisterTests( c : MemShiftRegister[Vec[SInt]] ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 1000

  val tmpsr = ArrayBuffer[List[BigInt]]()
  for ( i <- 0 until cycs ) {
    tmpsr += List.fill( c.io.in.size ) {
      val x = myRand.nextInt( 1 << 16 )
      if ( x >= ( 1 << 15 ) )
        BigInt( x - (1 << 16))
      else
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
      for ( i <- 0 until c.io.in.size )
        expect( c.io.out( i ), tmpsr( outIdx )(i) )
      outIdx += 1
    }
  }
}

class MemShiftRegisterSuite extends ChiselFlatSpec {

  behavior of "MemShiftRegister"
  backends foreach {backend =>
    it should s"correctly compute the memshiftregister $backend" in {
      Driver(() => {
        new MemShiftRegister( Vec( 64, SInt( 16.W ).cloneType ), 32 )
      }, "verilator", true )( c => new MemShiftRegisterTests( c ) ) should be (true)
    }
  }
}

