/** This file is the interface for a convolution compute
  */

package binconcifar

import chisel3._
import chisel3.util._

abstract class TriConvCompute( val throughput : Double, convShape : ( Int, Int, Int, Int ) ) extends Module {
  val dtype = Wire(SInt( 8.W ))
  type T = SInt

  if ( throughput >= 1 )
    assert( throughput == throughput.toInt,
      "Throughput must be integer when throughput > 1" )
  else
    assert( convShape._1 * throughput == ( convShape._1 * throughput ).toInt,
      "Must be integer number of outputs when throughput < 1" )
  val noOut = ( convShape._1 * throughput ).toInt
  val noIn = {
    if ( throughput < 1 )
      1
    else
      ( throughput ).toInt
  }

  val io = IO(new Bundle {
    val dataIn = Decoupled( Vec( noIn, Vec( convShape._2, Vec( convShape._3, Vec( convShape._4, dtype ))))).flip()
    val dataOut = Valid( Vec( noOut, dtype ))
  })

  def latency : Int
}
