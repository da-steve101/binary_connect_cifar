
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
    val dataOut = Decoupled( Vec( 1, dtype ) )
  })

  // just add a reg on input as don't have IO
  val bitsIn = io.dataIn.bits
  val vldIn = io.dataIn.valid
  val rdy = io.dataOut.ready

  // pass IO to blank Vgg7
  private val vgg = Module( new Vgg7( dtype ) )
  vgg.io.dataIn.bits := bitsIn
  vgg.io.dataIn.valid := vldIn
  io.dataIn.ready := vgg.io.dataIn.ready

  // Need to pipeline the mux
  val dataInAsUInt = vgg.io.dataOut.bits.asInstanceOf[Vec[SInt]].map( _.asUInt() ).reduce( _ ## _ )
  val queueIOIn = Wire( Decoupled( dataInAsUInt.cloneType ) )
  queueIOIn.bits := dataInAsUInt
  queueIOIn.valid := vgg.io.dataOut.valid

  val queueIOOut = Queue( queueIOIn, 4 )
  vgg.io.dataOut.ready := queueIOIn.ready

  val sintOut = Wire( vgg.io.dataOut.bits.cloneType )
  val dtypeWidth = dtype.getWidth
  for ( i <- 0 until vgg.io.dataOut.bits.size )
    sintOut( vgg.io.dataOut.bits.size - i - 1 ) := queueIOOut.bits((i+1)*dtypeWidth - 1, i*dtypeWidth).asSInt()

  val muxLyr = Module( new MuxLayer( dtype, 256, 4 ) )
  muxLyr.io.dataIn.bits := sintOut
  muxLyr.io.dataIn.valid := queueIOOut.valid
  queueIOOut.ready := muxLyr.io.dataIn.ready

  val bufferedSource_weights_fc = scala.io.Source.fromFile("src/main/resources/fc_1024_weights.csv")
  val weights_raw_fc = bufferedSource_weights_fc.getLines.toList
  val weights_fc = weights_raw_fc.map( _.split(",").toList.map( x => x.toInt ).toList ).transpose

  val dense = Module( new DenseLayer( dtype, 4, weights_fc ) )
  dense.io.dataIn <> muxLyr.io.dataOut

  val muxLyr_2 = Module( new MuxLayer( dtype, 1024, 1 ) )
  muxLyr_2.io.dataIn <> dense.io.dataOut
  dense.io.dataOut.ready := true.B // should always be ready ...

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

  dense_2.io.dataOut.ready := true.B // just set as true as should always be ready ...
  val outputRegs = Reg( dense_2.io.dataOut.bits.cloneType )
  val outCntr = RegInit( 0.U( 4.W ) )
  io.dataOut.valid := false.B
  when ( dense_2.io.dataOut.valid ) {
    outputRegs := dense_2.io.dataOut.bits
    outCntr := 1.U
  }
  when ( outCntr > 0.U && rdy ) {
    outCntr := outCntr + 1.U
    io.dataOut.valid := true.B
  }
  when ( outCntr >= 10.U && rdy ) {
    outCntr := 0.U
  }
  io.dataOut.bits(0) := outputRegs( outCntr - 1.U ) // NB: no need to mult scaling factor into softmax
  // muxLyr.io.dataOut <> io.dataOut
}
