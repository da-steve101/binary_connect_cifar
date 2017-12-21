
package aws

import chisel3._
import chisel3.util._
import binconcifar.MuxLayer
import binconcifar.DenseLayer
import binconcifar.DenseScale
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
  val convBits = 4
  val abFracBits = 6

  val io = IO( new Bundle {
    val dataIn = Flipped(Decoupled( Vec( 3, dtype ) ))
    val dataOut = Decoupled( Vec( 10, dtype ) )
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

  val dense = Module( new DenseLayer( dtype, 4, weights_fc ) )
  dense.io.dataIn <> muxLyr.io.dataOut

  val muxLyr_2 = Module( new MuxLayer( dtype, 1024, 1 ) )
  muxLyr_2.io.dataIn <> dense.io.dataOut

  // need a BN scale and shift then relu
  val bufferedSource_ab = scala.io.Source.fromFile("src/main/resources/fc_1024_ab.csv")
  val ab_raw = bufferedSource_ab.getLines.toList
  val ab = ab_raw.map( _.split(",").toList.map( x => math.round( x.toFloat * ( 1 << abFracBits ) ).toInt ) )

  val scale = Module( new DenseScale( dtype, ab(0), ab(1), convBits, abFracBits ) )
  scale.io.dataIn <> muxLyr_2.io.dataOut

  val bufferedSource_weights_sm = scala.io.Source.fromFile("src/main/resources/softmax_weights.csv")
  val weights_raw_sm = bufferedSource_weights_sm.getLines.toList
  val weights_sm = weights_raw_sm.map( _.split(",").toList.map( x => x.toInt ).toList ).transpose

  val dense_2 = Module( new DenseLayer( dtype, 1, weights_sm ) )
  dense_2.io.dataIn <> scale.io.dataOut

  // muxLyr_2.io.dataOut <> io.dataOut
  dense_2.io.dataOut <> io.dataOut
}
