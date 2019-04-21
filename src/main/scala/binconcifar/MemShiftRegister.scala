// See LICENSE for license details.

package binconcifar

import chisel3._
import chisel3.util._

/** This is functionally identical to a ShiftRegister but uses a Memory for storage.
  * If the value of n <= 2 it will revert to the ShiftRegister implementation.
  */
object MemShiftRegister {

  /** Returns the n-cycle delayed version of the input signal with reset initialization.
    *
    * @param in input to delay
    * @param n number of cycles to delay
    * @param en enable the shift
    */
  def apply[T <: Data]( in : T, n : Int, en : Bool = true.B ) : T = {
    val memSR = Module( new MemShiftRegister( in, n ) )
    memSR.io.en := en
    memSR.io.in := in
    memSR.io.out
  }
}

/** Do not use this class directly, instead use [[chisel3.util.MemShiftRegister$]] Factory method
  */
class MemShiftRegister[T <: Data]( genType : T, val n : Int ) extends Module {
  val io = IO(new Bundle {
    val in = Input( genType.cloneType )
    val en = Input( Bool() )
    val out = Output( genType.cloneType )
  })

  // if genType is a vec then aggregate bits to UInt
  val grpedVec : Vec[Bits] = io.in.asInstanceOf[Vec[Bits]]
  val uintVec = grpedVec.map( _.asUInt() ).reduce( _ ## _ )
  val bitWidth = uintVec.getWidth / grpedVec.size

  if ( n <= 3 ) {
    io.out := ShiftRegister( io.in, n, io.en )
  } else {
    val myMem = Mem( n, uintVec.cloneType )

    val cntr = Counter( io.en, n )
    val readAddr = Wire( UInt( cntr._1.getWidth.W + 1.W ) )

    readAddr := cntr._1
    when ( io.en ) {
      readAddr := cntr._1 + 1.U
      when ( cntr._1 === ( n - 1 ).U ) {
        readAddr := 0.U
      }
    }

    when ( io.en ) {
      myMem( cntr._1 ) := uintVec
    }

    val uintOut = myMem( readAddr )
    // register without enable for output of BRAM
    val uintBuf = RegNext( uintOut )

    val sintOut = Wire( grpedVec.cloneType )
    for ( i <- 0 until grpedVec.size ) {
      val b : Bits = uintBuf((i+1)*bitWidth - 1, i*bitWidth)
      if ( sintOut.head.isInstanceOf[SInt] )
        sintOut( grpedVec.size - i - 1 ) := b.asSInt()
      else
        sintOut( grpedVec.size - i - 1 ) := b
    }

    io.out := sintOut
  }
}

