
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

  for ( op <- treeDefinition ) {
    if ( op(2) >= 0 ) {
      val a = treeNodes( op(1) ) << op(4).U
      val b = treeNodes( op(2) ) << op(5).U
      if ( op(3) == 1 )
        treeNodes( op(0) ) := RegNext( a + b )
      else if ( op(3) == 0 )
        treeNodes( op(0) ) := RegNext( a - b )
      else
        treeNodes( op(0) ) := RegNext( - a - b )
      Predef.assert( nodeDelays( op(1) ) == nodeDelays( op(2) ),
        "Tree adds values from different layers for op: " + op )
      nodeDelays( op(0) ) = nodeDelays( op(1) ) + 1
    } else {
      if ( op(1) >= 0 ) {
        val a = treeNodes( op(1) ) << op(4).U
        if ( op(3) == 1 )
          treeNodes( op(0) ) := RegNext( a )
        else
          treeNodes( op(0) ) := RegNext( - a )
        nodeDelays( op(0) ) = nodeDelays( op(1) ) + 1
      } else {
        treeNodes( op(0) ) := 0.S( 16.W )
        nodeDelays( op(0) ) = 0
      }
    }
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
