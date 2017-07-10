/** This file is the interface for a convolution compute
  */

package binconcifar

import chisel3._
import chisel3.util._

abstract class NNLayer( val throughput : Double, inSize : Int, outSize : Int ) extends Module {
  val dtype = Wire(SInt( 8.W ))
  type T = SInt

  if ( throughput >= 1 )
    assert( throughput == throughput.toInt,
      "Throughput must be integer when throughput > 1" )
  else
    assert( outSize * throughput == ( outSize * throughput ).toInt &&
      inSize * throughput == ( inSize * throughput ).toInt,
      "Must be integer number of IO when throughput < 1" )
  val noOut = ( outSize * throughput ).toInt
  val noIn = ( inSize * throughput ).toInt

  val io = IO(new Bundle {
    val dataIn = Decoupled( Vec( noIn, dtype ) ).flip()
    val dataOut = Valid( Vec( noOut, dtype ) )
  })

  def latency : Int

  /** Convert the in io to a proper size
    */
  protected def inIOToVVV( dim1 : Int, dim2 : Int ) : Vec[Vec[Vec[T]]] = {
    val vecTmp = Vec( io.dataIn.bits.grouped( dim2 ).toList.map( Vec( _ ) ) )
    Vec( vecTmp.grouped( dim1 ).toList.map( Vec( _ ) ) )
  }
}
