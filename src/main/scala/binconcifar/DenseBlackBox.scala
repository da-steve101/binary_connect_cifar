package binconcifar

import chisel3._
import chisel3.util._
import java.io._

// blackbox to implement ROM
class DenseBlackBox(
  weights : Seq[Seq[Int]],
  tPut : Int,
  weightsBW : Int = 2
) extends BlackBox with HasBlackBoxInline  {

  val uintWeights = {
    if ( weightsBW == 2 ) {
      weights.map( nums => {
        nums.grouped( tPut ).map( grp => {
          grp.map( x => {
            if ( x == 1 )
              1
            else if ( x == -1 )
              3
            else
              0
          }).zipWithIndex.map( xi => {
            xi._1 << 2 * xi._2
          }).sum
        }).toList
      }).toList
    } else {
      weights.map( nums => {
        nums.grouped( tPut ).map( grp => {
          grp.map( x => {
            if ( x < 0 )
              ( 1 << weightsBW ) + x
            else
              x
          }).zipWithIndex.map( xi => {
            xi._1 << ( weightsBW * xi._2 )
          }).sum
        }).toList
      }).toList
    }
  }

  val weightsWidth = tPut * weightsBW // 2 bits per weight
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

  setInline( "/" + desiredName + ".v",
    module_dec + module_body
  )
}
