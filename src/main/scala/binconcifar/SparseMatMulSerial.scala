
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

  val io = IO( new Bundle {
    val dataIn = Flipped(Decoupled( Vec( noInputs, dtype.cloneType ) ))
    val dataOut = Decoupled( Vec( outputIdxs.size, dtype.cloneType ) )
  })

  val inWidth = dtype.cloneType.getWidth
  val nIter = inWidth / bitWidth
  val log2BW = log2Ceil( bitWidth )
  val log2Iter = log2Ceil( nIter )

  val nibbleCntrs = List.fill( 11 ) { RegInit( 0.U( log2Iter.W ) ) }
  val nibbleCntr = nibbleCntrs.head

  val almostRdy = RegNext( nibbleCntr === (( 1 << log2Iter) - 1).U || nibbleCntr === 0.U )
  val iterDone = RegInit( false.B )
  iterDone := ( nibbleCntr === (( 1 << log2Iter) - 2).U )
  val rdy = ( iterDone || ( !io.dataIn.valid && almostRdy ) )
  io.dataIn.ready := rdy

  for ( nc <- nibbleCntrs ) {
    when ( nc > 0.U || io.dataIn.valid ) {
      nc := nc + 1.U
    }
  }
  val dataInBits = io.dataIn.bits.toList.zipWithIndex.map( di => {
    val thisNibble = Reg( 0.U( bitWidth.W ).cloneType )
    val nc = nibbleCntrs( 1 + ( di._2 % ( nibbleCntrs.size - 1 ) ) )
    for ( x <- 0 until nIter ) {
      val dx = di._1( bitWidth * ( x + 1 ) - 1, bitWidth*x )
      if ( x > 0 ) {
        when ( nc === x.U ) {
          thisNibble := dx
        }
      } else
          thisNibble := dx
    }
    RegNext( thisNibble )
  })
  val startReg0 = ShiftRegister( nibbleCntr === 0.U && io.dataIn.valid, 1, false.B, true.B )

  val startRegs = ArrayBuffer[Bool]()
  startRegs.append( startReg0 )

  /*
  val dataNibble = Reg( Vec( noInputs, 0.U( bitWidth.W ).cloneType ) )
  for ( x <- 0 until nIter ) {
    for ( i <- 0 until noInputs ) {
      val thisNibble = ShiftRegister( io.dataIn.bits( i )( bitWidth * ( x + 1 ) - 1, bitWidth*x ), 1 + fanout )
      if ( x > 0 ) {
        when ( nibReg === x.U ) {
          dataNibble( i ) := thisNibble
        }
      } else
          dataNibble( i ) := thisNibble
    }
  }
   */

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

  val latency = treeLatency + 2 + nIter - 1

  io.dataOut.bits := Vec( unnibble )

  val vld = RegInit( false.B )
  vld := ( nibCntr === ( nIter - 1 ).U )
  io.dataOut.valid := vld

}
