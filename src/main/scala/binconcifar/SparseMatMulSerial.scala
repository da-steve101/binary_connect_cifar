
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class SparseMatMulSerial(
  dtype : SInt,
  val treeDefinition : Seq[Seq[Int]],
  val outputIdxs : Seq[Int],
  val bitWidth : Int,
  val fanout : Int = 0
) extends Module {

  val noInputs = treeDefinition.head.head
  val inType = UInt( width = bitWidth.W )

  val io = IO( new Bundle {
    val dataIn = Flipped(Valid( Vec( noInputs, inType.cloneType ) ))
    val dataOut = Valid( Vec( outputIdxs.size, dtype.cloneType ) )
  })

  val inWidth = dtype.cloneType.getWidth
  val nIter = inWidth / bitWidth
  val log2BW = log2Ceil( bitWidth )
  val log2Iter = log2Ceil( nIter )

  val nibbleCntr = RegInit( 0.U( log2Iter.W ) )

  when ( nibbleCntr > 0.U || io.dataIn.valid ) {
    nibbleCntr := nibbleCntr + 1.U
  }
  val dataInBits = ShiftRegister( io.dataIn.bits, 2 )
  val startReg0 = ShiftRegister( nibbleCntr === 0.U && io.dataIn.valid, 1, false.B, true.B )

  val startRegs = ArrayBuffer[Bool]()
  startRegs.append( startReg0 )

  val noNodes = treeDefinition.last.head + 1
  val treeNodes = Wire( Vec( noNodes, 0.U( bitWidth.W ).cloneType ) )

  val nodeDelays = ArrayBuffer.fill( noNodes ) { 0 }

  // default to 0
  for ( i <- 0 until noNodes )
    treeNodes( i ) := 0.U( 4.W )

  // connect inputs
  for ( d <- dataInBits.zipWithIndex )
    treeNodes( d._2 ) := d._1

  def compute_op( op_code : Int, a : UInt, b : UInt, start : Bool ) : UInt = {
    if ( op_code == 0 || op_code == 1 )
      return SerialAdder.negate( a, b, start, bitWidth )._1
    if ( op_code == 2 || op_code == 3 )
      return SerialAdder.sub( b, a, start, bitWidth )._1
    if ( op_code == 4 || op_code == 5 )
      return SerialAdder.sub( a, b, start, bitWidth )._1
    return SerialAdder.add( a, b, start, bitWidth )._1
  }

  def get_shift_val( op_idx : Int, shift_no : Int ) : UInt = {
    if ( op_idx < 0 )
      return 0.U
    val start = startRegs( nodeDelays( op_idx ) )
    if ( shift_no == 0 )
      return treeNodes( op_idx )
    if ( shift_no > 0 ) {
      val prevBits = Reg( 0.U( shift_no.W ).cloneType )
      prevBits := treeNodes( op_idx )( bitWidth - 1, bitWidth - shift_no )
      val outputBits = Wire( 0.U( bitWidth.W ).cloneType )
      outputBits := treeNodes( op_idx )(bitWidth - shift_no - 1,0) ## prevBits
      when ( start ) {
        outputBits := treeNodes( op_idx )(bitWidth - shift_no - 1,0) ## 0.U( shift_no.W )
      }
      return outputBits
    }
    // NOT ALLOWED
    Predef.assert( false, "Cannot do negative shifts" )
    return treeNodes( op_idx ) >> ( - shift_no ).U
  }

  for ( op <- treeDefinition ) {
    Predef.assert( op(3) == -1, "Cannot do 3 input adds" )
    Predef.assert( ( op(2) < 0 || nodeDelays( op(1) ) == nodeDelays( op(2) ) ) &&
      ( op(3) < 0 || nodeDelays( op(1) ) == nodeDelays( op(3) ) ),
      "Tree adds values from different layers for op: " + op )
    nodeDelays( op(0) ) = nodeDelays( op(1) ) + 1
    if ( startRegs.size <= nodeDelays( op(0) ) + 1 )
      startRegs.append( RegNext( startRegs.last ) )
    val a = get_shift_val( op(1), op(5) )
    val b = get_shift_val( op(2), op(6) )
    treeNodes( op(0) ) := {
      compute_op( op(4), a, b, startRegs( nodeDelays( op(0) ) - 1 ) )
    }
  }
  val outputs = outputIdxs.map( i => {
    if ( i < 0 )
      ( 0.U( bitWidth.W ), -1 )
    else
      ( treeNodes( i ), nodeDelays( i ) )
  })

  val treeLatency = outputs.map( _._2 ).max

  val outputsAligned = outputs.map( x => {
    if ( x._2 < 0 )
      x._1
    else
      ShiftRegister( x._1, treeLatency - x._2 )
  })

  val nibCntr = RegInit( 0.U( log2Iter.W ) )
  when ( startRegs.last || nibCntr > 0.U ) {
    nibCntr := nibCntr + 1.U
  }

  val unnibble = outputsAligned.map( x => {
    val outReg = Reg( Vec( nIter, 0.U( bitWidth.W ).cloneType ) )
    for ( i <- 0 until nIter ) {
      when ( nibCntr === i.U ) {
        outReg( i ) := x
      }
    }
    outReg.reverse.reduce( _ ## _ ).asTypeOf( dtype )
  })

  val srOut = 1
  val latency = treeLatency + 2 + nIter - 1 + srOut

  io.dataOut.bits := ShiftRegister( Vec( unnibble ), 1 )

  val vld = ShiftRegister( nibCntr === ( nIter - 1 ).U, srOut + 1, false.B, true.B )
  io.dataOut.valid := vld

}
