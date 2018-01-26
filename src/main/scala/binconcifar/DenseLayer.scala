
package binconcifar

import chisel3._
import chisel3.util._
import java.io._

// blackbox to implement ROM
private class DenseBlackBox(
  weights : Seq[Seq[Int]],
  tPut : Int
) extends BlackBox with HasBlackBoxResource  {

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
  val addrLen = log2Ceil( vecSize )
  val dataLen = outVecSize * weightsWidth

  val io = IO( new Bundle {
    val clock = Input( Clock() )
    val readAddr = Input( UInt( addrLen.W ) )
    val out = Output( UInt( dataLen.W ) )
  })

  def sha256Hash(text: String) : String = {
    val sha256 = java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))
    val ashex = String.format("%064x", new java.math.BigInteger(1, sha256 ))
    ashex.take(10)
  }

  val uintMem = ( 0 until vecSize ).map( i => {
    uintWeights.zipWithIndex.map( x => {
      BigInt( x._1(i) ) << ( weightsWidth * x._2 )
    }).sum
  }).toList.map( x => {
    dataLen.toString() + "'h" + x.toString(16)
  })

  val mem_init = uintMem.zipWithIndex.map( m => {
    "rom_uints[" + m._2 + "] = " + m._1 + ";\n"
  }).reduce( _ + _ )


  val rom_txt = """
reg [""" + ( dataLen - 1 ) + """:0] rom_uints [""" + (vecSize - 1) + """:0];
initial
begin
""" + mem_init + """end
"""

  val register_txt = """
reg [""" + ( dataLen - 1 ) + """:0] outputReg;
assign out = outputReg;
always @(posedge clock)
begin
  outputReg <= rom_uints[readAddr];
end
endmodule
"""

  val module_body = rom_txt + register_txt

  val hash = sha256Hash( module_body )

  val module_dec = """
module DenseBlackBox""" + hash + """(
  input clock,
  input [""" + ( addrLen - 1 ) +""":0] readAddr,
  output [""" + ( dataLen  - 1 ) + """:0] out
);
"""

  override def desiredName = "DenseBlackBox" + hash

  val filename = "src/main/verilog/" + desiredName + ".v"
  val pw = new PrintWriter(new File( filename ))
  pw.write( module_dec )
  pw.write( module_body )
  pw.close
  setResource( "/" + desiredName + ".v" )
}

private class MultiplyAccumulate(
  activationsType : SInt,
  weightsWidth : Int,
  noIn : Int
) extends Module {

  val io = IO( new Bundle {
    val activations = Input( Vec( noIn, activationsType ) )
    val weights = Input( UInt( weightsWidth.W ) )
    val sum = Output( activationsType.cloneType )
  })

  val actMult = io.activations.map( av => {
    val r = Reg( av.cloneType )
    r := 0.S
    r
  }).toList // actMult with 0 default

  for ( i <- 0 until weightsWidth ) {
    val idx = ( i / 2 ).toInt
    if ( i % 2 == 0 ) {
      when ( io.weights( i ) === 1.U ) {
        actMult( idx ) := io.activations( idx )
      }
    } else {
      when ( io.weights( i ) === 1.U ) {
        actMult( idx ) := -io.activations( idx )
      }
    }
  }

  val latency = log2Ceil( noIn ) + 1

  var numsToSum = actMult

  while( numsToSum.size > 1 ) {
    numsToSum = numsToSum.grouped( 2 ).map( grp => {
      val toSum = grp.reduce( _ + _ )
      RegNext( toSum )
    }).toList
  }
  io.sum := numsToSum.head
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

  val weightsWidth = tPut * 2
  val weightType = UInt( weightsWidth.W )

  val cntr = RegInit( 0.U( log2Ceil( weights.head.size / tPut ).W ) )
  when ( io.dataIn.valid ) {
    cntr := cntr + 1.U
  }

  // read tPut of them each cycle from the RAM
  val currWeights = Wire( Vec( weights.size, weightType ) )
  // private val weightsRAM = Module( new DenseWeights( weights, tPut ) )
  private val weightsRAM = Module( new DenseBlackBox( weights, tPut ) )
  weightsRAM.io.readAddr := RegNext( cntr )
  weightsRAM.io.clock := clock
  for ( i <- 0 until weights.size )
    currWeights( i ) := weightsRAM.io.out( weightsWidth*(i+1) - 1, weightsWidth*i )

  val currActs = RegNext( io.dataIn.bits )

  // add pipeline stages before fanout
  val delayWeights = ShiftRegister( currWeights, 2 )
  val delayActs = ShiftRegister( currActs, 3 )

  val summations = delayWeights.map( w => {
    val mac = Module( new MultiplyAccumulate( dtype, weightsWidth, tPut ) )
    mac.io.activations := delayActs
    mac.io.weights := w
    ( mac.io.sum, mac.latency )
  }).toList

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
