
package binconcifar

import chisel3._
import chisel3.experimental._ // To enable experimental features

object ternary_add_sub_prim {
  def make_three_input_add_sub(
    data_width : Int,
    clock : Clock,
    x : SInt,
    y : SInt,
    z : SInt,
    sub_x : Boolean,
    sub_y : Boolean,
    sub_z : Boolean
  ) : SInt = {
    val all_sub = sub_x && sub_y && sub_z
    // put false first
    val sorted_sub = List( ( x, sub_x ), ( y, sub_y ), ( z, sub_z ) ).sortBy( _._2 )
    val mod_sub_y = sorted_sub(1)._2 && !all_sub
    val mod_sub_z = sorted_sub(2)._2 && !all_sub
    val tasp = Module( new ternary_add_sub_prim( data_width, mod_sub_y, mod_sub_z ) )
    tasp.io.clk_i := clock
    tasp.io.x_i := sorted_sub(0)._1
    tasp.io.y_i := sorted_sub(1)._1
    tasp.io.z_i := sorted_sub(2)._1
    if ( all_sub )
      - tasp.io.sum_o
    else
      tasp.io.sum_o
  }

  def make_three_input_add_sub_sim(
    data_width : Int,
    clock : Clock,
    x : SInt,
    y : SInt,
    z : SInt,
    sub_x : Boolean,
    sub_y : Boolean,
    sub_z : Boolean
  ) : SInt = {
    val res = {
      if ( sub_x ) -x else x
    } + {
      if ( sub_y ) -y else y
    } + {
      if ( sub_z ) -z else z
    }
    RegNext( res )
  }

}

class ternary_add_sub_prim (
  data_width : Int,
  subtract_y : Boolean,
  subtract_z : Boolean,
  use_output_ff : Boolean = true,
  is_signed : Boolean = true
) extends BlackBox( Map(
  "input_word_size" -> data_width,
  "subtract_y" -> RawParam({
    if ( subtract_y )
      "True"
    else
      "False"
  }),
  "subtract_z" -> RawParam({
    if ( subtract_z )
      "True"
    else
      "False"
  }),
  "use_output_ff" -> RawParam({
    if ( use_output_ff )
      "True"
    else
      "False"
  }),
  "is_signed" -> RawParam({
    if ( is_signed )
      "True"
    else
      "False"
  })
)) {

  val io = IO(new Bundle {
    val clk_i = Input( Clock() )
    val x_i = Input( SInt( data_width.W ) )
    val y_i = Input( SInt( data_width.W ) )
    val z_i = Input( SInt( data_width.W ) )
    val sum_o = Output( SInt( data_width.W ) )
  })
}
