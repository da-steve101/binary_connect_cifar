package binconcifar

import chisel3._
import chisel3.util._

class DenseScale(
  dtype : SInt,
  a : Seq[Int],
  b : Seq[Int],
  val convPrec : Int,
  val fracPrec : Int
) extends Module {

  val io = IO( new Bundle {
    val dataIn = Flipped( Decoupled( Vec( 1, dtype ) ) )
    val dataOut = Decoupled( Vec( 1, dtype ) )
  })

  io.dataIn.ready := io.dataOut.ready

  val cntrBits = log2Ceil( a.size )

  val scaleCntr = RegInit( 0.U( cntrBits.W ) )
  when ( io.dataIn.valid && io.dataOut.ready ) {
    scaleCntr := scaleCntr + 1.U
  }

  val aVec = Wire( Vec( a.size, dtype ) )
  val bVec = Wire( Vec( b.size, dtype ) )
  for ( x <- a.zipWithIndex )
    aVec( x._2 ) := x._1.S
  for ( x <- b.zipWithIndex )
    bVec( x._2 ) := ( x._1 << convPrec ).S

  val currAct = Reg( dtype )
  val currA = Reg( dtype )
  val currB = Reg( dtype )
  currAct := io.dataIn.bits(0)
  currA := aVec( scaleCntr )
  currB := bVec( scaleCntr )

  val bDelayed = RegNext( currB )

  val scale = RegNext( currAct * currA )
  val shift = RegNext( scale + bDelayed )
  val output = shift >> fracPrec.U
  val relu = Reg( dtype )
  relu := 0.S
  when ( shift > 0.S ) {
    relu := output
  }

  val latency = 4

  io.dataOut.bits(0) := relu
  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )
}
