
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

  def negate(
    a : UInt,
    b : UInt,
    start : Bool,
    bitWidth : Int
  ) : ( UInt, Bool ) = {
    computeOp( a, b, start, false, bitWidth, true )
  }


  private def computeOp(
    a : UInt,
    b : UInt,
    start : Bool,
    isAdd : Boolean,
    bitWidth : Int,
    negate : Boolean = false
  ) : ( UInt, Bool ) = {
    val serAdd = Module( new SerialAdder( isAdd, bitWidth, negate ) )
    serAdd.io.a := a
    serAdd.io.b := b
    serAdd.io.start := start
    ( serAdd.io.out, serAdd.io.startOut )
  }
}

class SerialAdder( add : Boolean, bitWidth : Int, negate : Boolean = false ) extends Module {
  val io = IO( new Bundle {
    val a = Input( UInt( bitWidth.W ) )
    val b = Input( UInt( bitWidth.W ) )
    val start = Input( Bool() )
    val out = Output( UInt( bitWidth.W ) )
    val startOut = Output( Bool() )
  })

  val bInv = {
    if ( add && !negate )
      io.b
    else
      ~io.b
  }
  val aPad = {
    if ( !negate )
      0.U( 1.W ) ## io.a
    else
      1.U( 1.W ) ## ~io.a
  }
  val carry_init = {
    if ( add )
      0.U( 1.W )
    else if ( negate )
      2.U( 2.W )
    else
      1.U( 1.W )
  }
  val carry_in = Reg( 0.U( 1.W ).cloneType )
  val tmp = Wire( UInt( ( 1 + bitWidth ).W ) )

  val carry_use = Wire( carry_init.cloneType )
  if ( negate )
    carry_use := 0.U( 1.W ) ## carry_in
  else
    carry_use := carry_in
  when ( io.start ) {
    carry_use := carry_init
  }

  tmp := ( aPad + bInv ) + carry_use

  carry_in := tmp(bitWidth)
  io.out := RegNext( tmp(bitWidth - 1,0) )
  io.startOut := RegNext( io.start )
}
