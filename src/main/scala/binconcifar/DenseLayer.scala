
package binconcifar

import chisel3._
import chisel3.util._

class DenseLayer( dtype : SInt, val tPut : Int, weights : Seq[Seq[Int]] ) extends Module {
  val fracBits = 4
  val noOut = weights.size
  val noIn = weights.head.size
  val imgSize = 4

  val io = IO( new Bundle {
    val dataIn = Flipped( Decoupled( Vec( tPut, dtype ) ) )
    val dataOut = Decoupled( Vec( noOut, dtype ) )
  })

  // store the weights in a RAM
  val weightsWidth = tPut * 2
  val weightType = UInt( weightsWidth.W )
  val weightsRAM = Reg( Vec( noOut, Vec( noIn / tPut, weightType ) ) )

  for ( nums <- weights.zipWithIndex ) {
    for( grp <- nums._1.grouped( tPut ).zipWithIndex ) {
      val weights_val = grp._1.map( x => {
        if ( x == 1 )
          1
        else if ( x == -1 )
          3
        else
          0
      }).zipWithIndex.map( xi => {
        xi._1 << xi._2
      }).sum
      val weightsVec = weights_val.U( weightsWidth.W )
      weightsRAM( nums._2 )( grp._2 ) := weightsVec
    }
  }

  val cntr = RegInit( 0.U( log2Ceil( weights.head.size / tPut ).W ) )
  when ( io.dataIn.valid && io.dataOut.ready ) {
    cntr := cntr + 1.U
  }

  // read tPut of them each cycle from the RAM
  val currWeights = Reg( Vec( weights.size, weightType ) )
  for ( i <- 0 until weights.size )
    currWeights( i ) := weightsRAM( i )( cntr )

  val currActs = RegNext( io.dataIn.bits )

  def pipelinedSumSInts( numsToSum : Seq[SInt] ) : ( SInt, Int ) = {
    if ( numsToSum.size == 1 )
      return ( numsToSum.head, 0 )
    val res = numsToSum.grouped( 2 ).map( grp => {
      val toSum = grp.reduce( _ + _ )
      RegNext( toSum )
    }).toList
    val nextRes = pipelinedSumSInts( res )
    ( nextRes._1, nextRes._2 + 1 )
  }

  def computeMACs( weightsVec : UInt, activationsVec : Seq[SInt] ) : ( SInt, Int ) = {
    val actMult = activationsVec.map( av => {
      val r = Reg( av.cloneType )
      r := 0.S
      r
    }).toList // actMult with 0 default
    for ( i <- 0 until weightsWidth ) {
      val idx = ( i / 2 ).toInt
      if ( i % 2 == 0 ) {
        when ( weightsVec( i ) === 1.U ) {
          actMult( idx ) := activationsVec( idx )
        }
      } else {
        when ( weightsVec( i ) === 1.U ) {
          actMult( idx ) := -activationsVec( idx )
        }
      }
    }
    val sumOut = pipelinedSumSInts( actMult )
    ( sumOut._1, sumOut._2 + 1 )
  }

  // add pipeline stages before fanout?
  val delayWeights = ShiftRegister( currWeights, 2 )
  val delayActs = ShiftRegister( currActs, 2 )

  val summations = delayWeights.map( w => {
    computeMACs( w, delayActs )
  } ).toList

  val tmpAct = RegNext( delayActs(0) )
  val tmpWeight = RegNext( delayWeights(0) )

  val sumLatency = summations.head._2 + 3

  val cummulativeSums = Reg( Vec( noOut, dtype.cloneType ) )

  // one less than delay so resets the cyc before ...
  val rst = ShiftRegister( cntr === 0.U, sumLatency + 1 )
  val done = RegInit( false.B )
  when ( cntr  === ((noIn/tPut) - 1).U ) {
    done := true.B
  }
  val vld = ShiftRegister( io.dataIn.valid && io.dataOut.ready, sumLatency )

  for ( s <- summations.map( _._1 ).zipWithIndex ) {
    when ( vld ) {
      cummulativeSums( s._2 ) :=  cummulativeSums( s._2 ) + s._1
    }
    when ( rst ) {
      cummulativeSums( s._2 ) := 0.S
    }
  }

  val output_nums = Reg( Vec( noOut, dtype ) )
  val out_vld_reg = RegInit( false.B )
  when ( rst && done ) {
    output_nums := cummulativeSums
    out_vld_reg := true.B
    done := false.B
  }
  when ( out_vld_reg && io.dataOut.ready ) {
    out_vld_reg := false.B
  }

  io.dataIn.ready := io.dataOut.ready
  io.dataOut.valid := out_vld_reg
  io.dataOut.bits := output_nums

}
