
package binconcifar

import chisel3._

class Xilinx_GenericAddSub_16_dss extends BlackBox {
  val io = IO(new Bundle {
    val x_i = Input( SInt( 16.W ) )
    val y_i = Input( SInt( 16.W ) )
    val neg_x_i = Input( Bool() )
    val neg_y_i = Input( Bool() )
    val sum_o = Output( SInt( 16.W ) )
    val c_o = Output( Bool() )
  })
}

class GenericAddSub16( is_sim : Boolean = true ) extends Module {

  val io = IO( new Bundle {
    val x_i = Input( SInt( 16.W ) )
    val y_i = Input( SInt( 16.W ) )
    val x_i_neg = Input( Bool() )
    val x_i_zero = Input( Bool() )
    val y_i_neg = Input( Bool() )
    val y_i_zero = Input( Bool() )
    val sum_o = Output( SInt( 16.W ) )
  })

  val x_i_reg = Reg( SInt( 16.W ) )
  val y_i_reg = Reg( SInt( 16.W ) )
  x_i_reg := io.x_i
  y_i_reg := io.y_i
  // Have reset for reg based on weights
  when ( io.x_i_zero ) {
    x_i_reg := 0.S
  }
  when ( io.y_i_zero ) {
    y_i_reg := 0.S
  }
  val x_i_neg_reg = RegNext( io.x_i_neg )
  val y_i_neg_reg = RegNext( io.y_i_neg )

  if ( is_sim ) {
    val x_i = Wire( SInt( 16.W ) )
    val y_i = Wire( SInt( 16.W ) )
    x_i := x_i_reg
    y_i := y_i_reg
    when ( x_i_neg_reg ) {
      x_i := -x_i_reg
    }
    when ( y_i_neg_reg ) {
      y_i := -y_i_reg
    }
    io.sum_o := RegNext( x_i + y_i )
  } else {
    val mul_add = Module( new Xilinx_GenericAddSub_16_dss )
    mul_add.io.x_i := x_i_reg
    mul_add.io.y_i := y_i_reg
    mul_add.io.neg_x_i := x_i_neg_reg
    mul_add.io.neg_y_i := y_i_neg_reg
    io.sum_o := RegNext( mul_add.io.sum_o )
  }
}
