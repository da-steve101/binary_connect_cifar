
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.MuxLayer
import scala.collection.mutable.ArrayBuffer

class MuxComputeTests( c : MuxLayer ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = 500

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 4 ) ).toInt )
  }

  val inputs = List.fill( cycs ) { List.fill( c.inSize ){ getRndFP() } }
  // val inputs = List.fill( cycs ) { ( 0 until c.inSize ).map( idx => BigInt( idx ) ).toList }
  poke( c.io.dataOut.ready, true.B )
  var input_cntr = c.noGrps
  var output_cyc_cntr = 0
  var output_grp_cntr = 0
  var input_cyc_cntr = 0
  for ( cyc <- 0 until cycs ) {
    val rdy = peek( c.io.dataIn.ready ) == 1
    for ( i <- 0 until c.inSize )
      poke( c.io.dataIn.bits(i), inputs( input_cyc_cntr )( i ) )
    if ( input_cntr >= c.noGrps - 1 && rdy ) {
      poke( c.io.dataIn.valid, true.B )
      input_cntr = 0
      input_cyc_cntr += 1
    } else {
      poke( c.io.dataIn.valid, false.B )
      input_cntr += 1
    }
    step( 1 )
    val vld = peek( c.io.dataOut.valid ) == 1
    if ( vld ) {
      for ( i <- 0 until c.outSize ) {
        expect( c.io.dataOut.bits( i ), inputs( output_cyc_cntr )( output_grp_cntr ) )
        output_grp_cntr += 1
      }
      if ( output_grp_cntr >= c.inSize - 1 ) {
        output_grp_cntr = 0
        output_cyc_cntr += 1
      }
    }
  }

}

class MuxLayerSuite extends ChiselFlatSpec {
  behavior of "MuxLayer"
  backends foreach {backend =>
    it should s"correctly compute the mux $backend" in {
      for ( sizes <- List[(Int, Int)](
        ( 128, 8 )
        /*( 256, 4 )*/
        /*( 1024, 1 )*/
      ) ) {
        val inSize = sizes._1
        val outSize = sizes._2
        Driver(() => {
          new MuxLayer( SInt( 16.W ), inSize, outSize )
        }, "verilator", true )( c => new MuxComputeTests( c ) ) should be (true)
      }
    }
  }
}
