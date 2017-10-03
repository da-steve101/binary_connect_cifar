package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class ScaleAndShift (
  conv_prec : Int,
  ab_prec : Int,
  val a : Seq[Int],
  val b : Seq[Int],
  tput : Double ) extends
    NNLayer( tput,
      a.size,
      a.size,
      tput.toInt )  {

  val to_mult = io.dataIn.bits.grouped( a.size ).toList
  val zero = 0.U.asTypeOf( dtype )
  val mult_res = to_mult.map( x => x.zipWithIndex.map( z => {
    val scale = Wire( dtype.cloneType )
    scale := RegNext( a( z._2 ).S * z._1 )
    val shift = RegNext( scale + ( b( z._2 ) << conv_prec ).S )
    val output = shift >> ab_prec.U
    val relu = Wire( dtype.cloneType )
    relu := zero
    when ( output >= zero ) {
      relu := output
    }
    RegNext( relu )
  }))

  val latency = 3

  val output = Vec( mult_res.reduce( _ ++ _ ) )
  io.dataOut.bits := output
  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )
  io.dataIn.ready := io.dataOut.ready
}
