
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
      0.U( 2.W ) ## ~io.a
  }
  val carry_init = {
    if ( add && !negate )
      0.U( 1.W )
    else if ( negate )
      2.U( 2.W )
    else
      1.U( 1.W )
  }
  val carry_in = Reg( carry_init.cloneType )
  val tmp = Wire( aPad.cloneType )

  tmp := ( aPad + bInv ) + carry_in

  if ( negate )
    carry_in := tmp(bitWidth + 1, bitWidth )
  else
    carry_in := tmp(bitWidth)

  when ( io.start ) {
    carry_in := carry_init
  }

  io.out := RegNext( tmp(bitWidth - 1,0) )
  io.startOut := RegNext( io.start )
}
