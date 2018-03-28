
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class SparseMatMul(
  dtype : SInt,
  val treeDefinition : Seq[Seq[Int]],
  val outputIdxs : Seq[Int]
) extends Module {

  val noInputs = treeDefinition.head.head

  val io = IO( new Bundle {
    val dataIn = Flipped(Decoupled( Vec( noInputs, dtype.cloneType ) ))
    val dataOut = Decoupled( Vec( outputIdxs.size, dtype.cloneType ) )
  })

  io.dataIn.ready := io.dataOut.ready

  val noNodes = treeDefinition.last.head + 1
  val treeNodes = Wire( Vec( noNodes, dtype.cloneType ) )

  val nodeDelays = ArrayBuffer.fill( noNodes ) { 0 }

  // default to 0
  for ( i <- 0 until noNodes )
    treeNodes( i ) := 0.S( 16.W )

  // connect inputs
  for ( d <- io.dataIn.bits.zipWithIndex )
    treeNodes( d._2 ) := d._1

  def compute_op( op_code : Int, a : SInt, b : SInt, c : SInt ) : SInt = {
    if ( op_code == 0 )
      return - a - b - c
    if ( op_code == 1 )
      return - a - b + c
    if ( op_code == 2 )
      return - a + b - c
    if ( op_code == 3 )
      return - a + b + c
    if ( op_code == 4 )
      return a - b - c
    if ( op_code == 5 )
      return a - b + c
    if ( op_code == 6 )
      return a + b - c
    return a + b + c
  }

  def compute_op_with_ternary_vhd( op_code : Int, a : SInt, b : SInt, c : SInt ) : SInt = {
    val sub_a = op_code < 4
    val sub_b = ( op_code % 4 ) < 2
    val sub_c = ( op_code % 2 ) < 1
    ternary_add_sub_prim.make_three_input_add_sub_sim(
      16, clock, a, b, c, sub_a, sub_b, sub_c
    )
  }

  def get_shift_val( op_idx : Int, shift_no : Int ) : SInt = {
    if ( op_idx < 0 )
      return 0.S
    if ( shift_no >= 0 )
      return treeNodes( op_idx ) << shift_no.U
    return treeNodes( op_idx ) >> ( - shift_no ).U
  }

  for ( op <- treeDefinition ) {
    val a = get_shift_val( op(1), op(5) )
    val b = get_shift_val( op(2), op(6) )
    val c = get_shift_val( op(3), op(7) )
    treeNodes( op(0) ) := {
      if ( op(2) >= 0 && op(3) >= 0 )
        compute_op_with_ternary_vhd( op(4), a, b, c )
      else
        RegNext( compute_op( op(4), a, b, c ) )
    }

    Predef.assert( ( op(2) < 0 || nodeDelays( op(1) ) == nodeDelays( op(2) ) ) &&
      ( op(3) < 0 || nodeDelays( op(1) ) == nodeDelays( op(3) ) ),
      "Tree adds values from different layers for op: " + op )
    nodeDelays( op(0) ) = nodeDelays( op(1) ) + 1
  }
  val outputs = outputIdxs.map( i => {
    if ( i < 0 )
      ( 0.S( 16.W ), -1 )
    else
      ( treeNodes( i ), nodeDelays( i ) )
  })

  val latency = outputs.map( _._2 ).max

  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )

  val outputsAligned = outputs.map( x => {
    if ( x._2 < 0 )
      x._1
    else
      ShiftRegister( x._1, latency - x._2 )
  })

  io.dataOut.bits := Vec( outputsAligned )

}
