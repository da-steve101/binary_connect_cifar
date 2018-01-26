package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

private class IndividualScaleAndShift(
  dtype : SInt,
  conv_prec : Int,
  ab_prec : Int,
  a : Int,
  b : Int
) extends Module {
  val io = IO( new Bundle{
    val actIn = Input( dtype.cloneType )
    val actOut = Output( dtype.cloneType )
  })

  val latency = 4

  val zero = 0.U.asTypeOf( dtype )

  val aTmp = Wire( dtype )
  aTmp := a.S( dtype.getWidth.W )
  val bTmp = Wire( dtype )
  bTmp := ( b << conv_prec ).S( dtype.getWidth.W )

  val zTmp = RegNext( io.actIn )

  val scale = RegNext( aTmp * zTmp )

  val shift = RegNext( scale + bTmp )

  val output = shift >> ab_prec.U
  val relu = Wire( dtype )

  relu := zero
  when ( output > zero ) {
    relu := output
  }
  io.actOut := RegNext( relu )
}

class ScaleAndShift (
  dtype : SInt,
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

  val to_mult = io.dataIn.bits.grouped( a.size ).toList

  val mult_res = to_mult.map( x => {
    x.zipWithIndex.map( z => {
      val iss = Module( new IndividualScaleAndShift(
        dtype,
        conv_prec,
        ab_prec,
        a( z._2 ),
        b( z._2 )
      ))
      iss.io.actIn := z._1
      iss.io.actOut
    })
  }).reduce( _ ++ _ )

  val latency = 4

  io.dataOut.bits := Vec( mult_res )
  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )
  io.dataIn.ready := true.B
}
