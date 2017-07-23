/** This file is the interface for a convolution compute
  */

package binconcifar

import chisel3._
import chisel3.util._

abstract class NNLayer( val throughput : Double, inSize : Int,
  outSize : Int, val noOut : Int ) extends Module {

  val dtype = Wire(SInt( 8.W ))
  type T = SInt

  if ( throughput >= 1 )
    assert( throughput == throughput.toInt,
      "Throughput must be integer when throughput > 1" )
  else
    assert( outSize * throughput == ( outSize * throughput ).toInt &&
      inSize * throughput == ( inSize * throughput ).toInt,
      "Must be integer number of IO when throughput < 1" )

  // noIn is defined as throughput
  val noIn = ( throughput ).toInt

  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled( Vec( inSize * noIn, dtype ) ))
    val dataOut = Decoupled( Vec( outSize * noOut, dtype ) )
    val vldMask = Output( Vec( noOut, Bool() ) )
  })

  def latency : Int

  /** Convert the in io to a proper size
    */
  protected def inIOToVVV( dim1 : Int, dim2 : Int ) : Vec[Vec[Vec[T]]] = {
    val vecTmp = Vec( io.dataIn.bits.grouped( dim2 ).toList.map( Vec( _ ) ) )
    Vec( vecTmp.grouped( dim1 ).toList.map( Vec( _ ) ) )
  }
}
