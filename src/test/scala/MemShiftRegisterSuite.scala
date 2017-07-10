
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.MemShiftRegister
import scala.collection.mutable.ArrayBuffer

class MemUserMod( val n : Int ) extends Module {
  val io = IO(new Bundle {
    val in = Input( UInt( 16.W ) )
    val en = Input( Bool() )
    val out = Output( UInt( 16.W ) )
  })

  val memSr = MemShiftRegister( io.in, n, io.en )
  io.out := memSr
}

class MemShiftRegisterTests( c : MemUserMod ) extends PeekPokeTester( c ) {
  val inputs = ArrayBuffer[BigInt]()
  val myRand = new Random
  val cycles = 3*( c.n + 10 )
  var outIdx = -c.n + 1
  for ( cyc <- 0 until cycles ) {
    val en = { myRand.nextInt(10) != 0 }
    val in = myRand.nextInt( 1 << 15 )
    poke( c.io.in, in )
    poke( c.io.en, en )
    step(1)
    if ( en ) {
      inputs += BigInt( in )
      if ( outIdx >= 0 )
        expect( c.io.out, inputs(outIdx) )
      outIdx += 1
    }
  }
}

class MemShiftRegisterSuite extends ChiselFlatSpec {
  behavior of "MemShiftRegisterSuite"
  backends foreach {backend =>
    it should s"correctly shift values using a memory $backend" in {
      for ( n <- List(1, 2, 3, 5, 25, 73) ) {
        Driver(() => {
          new MemUserMod( n )
        }, backend )( c => new MemShiftRegisterTests( c ) ) should be (true)
      }
    }
  }
}
