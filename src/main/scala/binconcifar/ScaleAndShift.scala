package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class ScaleAndShift[ T <: SInt] (
  dtype : T,
  conv_prec : Int,
  ab_prec : Int,
  val a : Seq[Int],
  val b : Seq[Int],
  tput : Double
) extends NNLayer(
  dtype,
  tput,
  a.size,
  a.size,
  tput.toInt ) {

  for ( d <- io.vldMask )
    d := false.B

  val to_mult = io.dataIn.bits.grouped( a.size ).toList
  val zero = 0.U.asTypeOf( dtype )
  val mult_res = to_mult.map( x => x.zipWithIndex.map( z => {
    val scale = Wire( dtype )
    val aTmp = Reg( dtype )
    aTmp := a( z._2 ).S.asInstanceOf[T]
    val zTmp = Reg( z._1.cloneType )
    zTmp := z._1
    val bTmp = Reg( dtype )
    bTmp := ( b( z._2 ) << conv_prec ).S.asInstanceOf[T]
    scale := RegNext( aTmp * zTmp )
    val shift = RegNext( scale + bTmp )
    val output = shift >> ab_prec.U
    val relu = Wire( dtype )
    relu := zero
    when ( output > zero ) {
      relu := output
    }
    RegNext( relu )
  }))

  val latency = 4

  val output = Vec( mult_res.reduce( _ ++ _ ) )
  io.dataOut.bits := output
  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )
  io.dataIn.ready := io.dataOut.ready
}
