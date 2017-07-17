
package binconcifar

import chisel3._
import chisel3.util._

object MemShiftRegister {

  def apply[ T <: Data ]( in : T, n : Int, en : Bool = true.B ) : T = {
    val memSR = Module( new MemShiftRegister( in, n ) )
    memSR.io.en := en
    memSR.io.in := in
    memSR.io.out
  }

}

class MemShiftRegister[ T <: Data ]( genType : T, n : Int ) extends Module {
  val io = IO(new Bundle {
    val in = Input( genType.cloneType )
    val en = Input( Bool() )
    val out = Output( genType.cloneType )
  })

  if ( n <= 2 ) {
    val reg1 = Reg( genType.cloneType )
    val reg2 = Reg( genType.cloneType )
    // use this as en is implemented differently in a shift register
    // in ShiftRegister en is just on input
    when ( io.en ) {
      reg1 := io.in
      reg2 := reg1
    }
    io.out := {
      if ( n == 2 )
        reg2
      else if ( n == 1 )
        reg1
      else
        io.in
    }
  } else {
    val myMem = SeqMem( n - 1, genType.cloneType )

    // put a register at the front and back
    val regTop = Reg( genType.cloneType )
    val cntr = Counter( io.en, n - 1 )
    val readAddr = Wire( UInt( cntr._1.getWidth.W + 1.W ) )

    readAddr := cntr._1 + 1.U
    when ( cntr._1 === ( n - 2 ).U ) {
      readAddr := 0.U
    }

    when ( io.en ) {
      regTop := io.in
      myMem.write( cntr._1, regTop )
    }
    io.out := myMem.read( readAddr, io.en )
  }
}
