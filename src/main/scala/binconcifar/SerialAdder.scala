
package binconcifar

import chisel3._

object SerialAdder {

  def add(
    a : UInt,
    b : UInt,
    start : Bool,
    bitWidth : Int
  ) : ( UInt, Bool ) = {
    computeOp( a, b, start, true, bitWidth )
  }

  def sub(
    a : UInt,
    b : UInt,
    start : Bool,
    bitWidth : Int
  ) : ( UInt, Bool ) = {
    computeOp( a, b, start, false, bitWidth )
  }

  private def computeOp(
    a : UInt,
    b : UInt,
    start : Bool,
    isAdd : Boolean,
    bitWidth : Int
  ) : ( UInt, Bool ) = {
    val serAdd = Module( new SerialAdder( isAdd, bitWidth ) )
    serAdd.io.a := a
    serAdd.io.b := b
    serAdd.io.start := start
    ( serAdd.io.out, serAdd.io.startOut )
  }
}

class SerialAdder( add : Boolean, bitWidth : Int ) extends Module {
  val io = IO( new Bundle {
    val a = Input( UInt( bitWidth.W ) )
    val b = Input( UInt( bitWidth.W ) )
    val start = Input( Bool() )
    val out = Output( UInt( bitWidth.W ) )
    val startOut = Output( Bool() )
  })

  val bInv = {
    if ( add )
      io.b
    else
      ~io.b
  }
  val carry_in = Reg( UInt( 1.W ) )
  val tmp = Wire( UInt( ( 1 + bitWidth ).W ) )

  val aPad = 0.U( 1.W ) ## io.a
  val carry_use = Wire( UInt( 1.W ) )
  carry_use := carry_in
  val carry_init = {
    if ( add )
      0.U( 1.W )
    else
      1.U( 1.W )
  }
  when ( io.start ) {
    carry_use := carry_init
  }

  tmp := ( aPad + bInv ) + carry_use

  carry_in := tmp(bitWidth)
  io.out := RegNext( tmp(bitWidth - 1,0) )
  io.startOut := RegNext( io.start )
}
