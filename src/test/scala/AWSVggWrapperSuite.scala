
package binconcifartests

import chisel3._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import aws.AWSVggWrapper

class AWSVggWrapperTests( c : AWSVggWrapper ) extends PeekPokeTester( c ) {
  poke( c.io.dataIn.valid, true )
  poke( c.io.dataOut.ready, true )
  poke( c.io.dataIn.bits(0), 0 )
  poke( c.io.dataIn.bits(1), 1 )
  poke( c.io.dataIn.bits(2), 2 )

  for ( i <- 0 until 1024 ) {
    step(1)
    for ( bitsOut <- c.io.dataOut.bits )
      peek( bitsOut )
    val vld = peek( c.io.dataOut.valid )
  }
}

class AWSVggWrapperSuite extends ChiselFlatSpec {

  behavior of "AWSVggWrapperSuite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => { new AWSVggWrapper  }, "verilator", false )( c =>
        new AWSVggWrapperTests( c ) ) should be (true)
    }
  }
}

