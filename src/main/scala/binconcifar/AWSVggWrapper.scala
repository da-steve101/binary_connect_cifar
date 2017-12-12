
package aws

import chisel3._
import chisel3.util._
import binconcifar.MuxLayer
import collection.mutable.ArrayBuffer

private class Vgg7( dtype : SInt ) extends Module {
  /*
   An empty module to delete and replace
   */
  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled( Vec( 3, dtype ) ))
    val dataOut = Decoupled( Vec( 256, dtype ) )
  })

  val vld = RegInit( false.B )
  vld := io.dataIn.valid
  for ( i <- 0 until 256 )
    io.dataOut.bits(i) := RegNext( io.dataIn.bits( i % 3 ) + (i - 128).S( 16.W ) )
  io.dataOut.valid := vld
  io.dataIn.ready := io.dataOut.ready

}

class DenseLayer( dtype : SInt, tPut : Int, weights : Seq[Seq[Int]] ) extends Module {
  val io = IO( new Bundle {
    val dataIn = Flipped( Decoupled( Vec( tPut, dtype ) ) )
    val dataOut = Decoupled( Vec( weights.size, dtype ) )
  })
}

class AWSVggWrapper extends Module {

  val dtype = SInt( 16.W )

  val io = IO( new Bundle {
    val dataIn = Flipped(Decoupled( Vec( 3, dtype ) ))
    val dataOut = Decoupled( Vec( 4, dtype ) )
  })

  // pass IO to blank Vgg7
  private val vgg = Module( new Vgg7( dtype ) )
  vgg.io.dataIn <> io.dataIn

  // Need to pipeline the mux

  val muxLyr = Module( new MuxLayer( dtype, 256, 4 ) )
  printf( "vgg.io.dataOut.bits = \n" )
  for ( d <- vgg.io.dataOut.bits )
    printf( "%d, ", d )
  printf( "\n" )
  printf( "io.dataIn.valid = %d\n", io.dataIn.valid )
  printf( "vgg.io.dataOut.valid = %d\n", vgg.io.dataOut.valid )
  muxLyr.io.dataIn <> vgg.io.dataOut
  muxLyr.io.dataOut <> io.dataOut
}
