
package aws

import chisel3._
import chisel3.util._
import binconcifar.MuxLayer
import binconcifar.DenseLayer
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
  for ( i <- 0 until io.dataOut.bits.size )
    io.dataOut.bits(i) := RegNext( io.dataIn.bits( i % 3 ) + (i - 128).S( 16.W ) )
  io.dataOut.valid := vld
  io.dataIn.ready := io.dataOut.ready

}

class AWSVggWrapper extends Module {

  val dtype = SInt( 16.W )

  val io = IO( new Bundle {
    val dataIn = Flipped(Decoupled( Vec( 3, dtype ) ))
    val dataOut = Decoupled( Vec( 1, dtype ) )
  })

  // pass IO to blank Vgg7
  private val vgg = Module( new Vgg7( dtype ) )
  vgg.io.dataIn <> io.dataIn

  // Need to pipeline the mux

  val dataQ = Queue( vgg.io.dataOut, 4 )

  val muxLyr = Module( new MuxLayer( dtype, 256, 4 ) )
  muxLyr.io.dataIn <> dataQ

  val bufferedSource_weights_fc = scala.io.Source.fromFile("src/main/resources/fc_1024_weights.csv")
  val weights_raw_fc = bufferedSource_weights_fc.getLines.toList
  val weights_fc = weights_raw_fc.map( _.split(",").toList.map( x => x.toInt ).toList ).transpose

  val dense = Module( new DenseLayer( SInt( 16.W ).cloneType, 4, weights_fc ) )
  dense.io.dataIn <> muxLyr.io.dataOut

  // need a BN scale and shift then relu

  val muxLyr_2 = Module( new MuxLayer( dtype, 1024, 1 ) )
  muxLyr_2.io.dataIn <> dense.io.dataOut

  muxLyr_2.io.dataOut <> io.dataOut

  /*
  val bufferedSource_weights_sm = scala.io.Source.fromFile("src/main/resources/softmax_weights.csv")
  val weights_raw_sm = bufferedSource_weights_sm.getLines.toList
  val weights_sm = weights_raw_sm.map( _.split(",").toList.map( x => x.toInt ).toList ).transpose

  val dense_2 = Module( new DenseLayer( SInt( 16.W ).cloneType, 1, weights_sm ) )
  dense_2.io.dataIn <> muxLyr_2.io.dataOut

  val muxLyr_3 = Module( new MuxLayer( dtype, 10, 1 ) )
  muxLyr_3.io.dataIn <> dense_2.io.dataOut

   */

}
