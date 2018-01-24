
package binconcifar

import chisel3._
import chisel3.util._

private class DenseWeights( weights : Seq[Seq[Int]], tPut : Int ) extends Module {

  val uintWeights = weights.map( nums => {
    nums.grouped( tPut ).map( grp => {
      grp.map( x => {
        if ( x == 1 )
          1
        else if ( x == -1 )
          2
        else
          0
      }).zipWithIndex.map( xi => {
        xi._1 << 2 * xi._2
      }).sum
    }).toList
  }).toList

  val weightsWidth = tPut * 2 // 2 bits per weight
  val outVecSize = uintWeights.size
  val vecSize = uintWeights.head.size

  val io = IO( new Bundle {
    val readAddr = Input( UInt( log2Ceil( vecSize ).W ) )
    val out = Output( UInt( (outVecSize * weightsWidth).W ) )
  })

  val uintMem = ( 0 until vecSize ).map( i => {
    val uintCombined = uintWeights.zipWithIndex.map( x => BigInt( x._1(i) ) << ( weightsWidth * x._2 ) ).sum
    uintCombined.U( ( outVecSize * weightsWidth ).W )
  }).toList

  val weightsROM = Vec( uintMem )
  val addrDelay = RegNext( io.readAddr )
  io.out := RegNext( weightsROM( addrDelay ) )
}

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

  val cntr = RegInit( 0.U( log2Ceil( weights.head.size / tPut ).W ) )
  when ( io.dataIn.valid ) {
    cntr := cntr + 1.U
  }

  // read tPut of them each cycle from the RAM
  val currWeights = Wire( Vec( weights.size, weightType ) )
  private val weightsRAM = Module( new DenseWeights( weights, tPut ) )
  weightsRAM.io.readAddr := cntr
  for ( i <- 0 until weights.size )
    currWeights( i ) := weightsRAM.io.out( weightsWidth*(i+1) - 1, weightsWidth*i )

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
  val delayActs = ShiftRegister( currActs, 3 )

  val summations = delayWeights.map( w => {
    computeMACs( w, delayActs )
  } ).toList

  val sumLatency = summations.head._2 + 4

  val cummulativeSums = Reg( Vec( noOut, dtype.cloneType ) )
  val rst = ShiftRegister( cntr === 0.U, sumLatency )
  val done = RegInit( false.B )
  when ( cntr  === ((noIn/tPut) - 1).U ) {
    done := true.B
  }
  when ( rst & done ) {
    done := false.B
  }
  val vld = ShiftRegister( io.dataIn.valid, sumLatency )

  for ( s <- summations.map( _._1 ).zipWithIndex ) {
    when ( vld ) {
      cummulativeSums( s._2 ) :=  cummulativeSums( s._2 ) + s._1
    }
    when ( rst ) {
      cummulativeSums( s._2 ) := 0.S
    }
    when ( rst & vld ) {
      cummulativeSums( s._2 ) := s._1
    }
  }

  io.dataIn.ready := true.B
  io.dataOut.valid := rst & done
  io.dataOut.bits := cummulativeSums

}
