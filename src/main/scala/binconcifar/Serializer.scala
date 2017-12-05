/** This file provides blocks for convienient vector manipulations
  */

package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object Serializer {
  def apply[T <: Bits]( genType : T, widthIn : Int, widthOut : Int ) : Serializer[T] = {
    val mySer = Module( new Serializer( genType, widthIn, widthOut ) )
    mySer.io.flush := false.B
    mySer
  }
}

/** This block provides a conversion from a vector of one width to another
  * The interface has a bits/valid designed to pull input from a fifo
  * The output is bits/valid
  */
class Serializer[T <: Bits]( genType : T, widthIn : Int, widthOut : Int) extends Module {

  if ( widthIn < 1 || widthOut < 1 )
    ChiselExecutionFailure("Widths of input and output vectors must be greater than 0")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

  val io = IO(new Bundle {
    val dataIn = Flipped( Decoupled( Vec( widthIn, genType.cloneType ) ) )
    val flush = Input( Bool() )
    val dataOut = Output( Valid( Vec( widthOut, genType.cloneType ) ) )
    val flushed = Output( Bool() )
  })
  val genWidth = genType.getWidth

  // compute GCD then attach to vec of combined
  val widthGcd = { if ( widthIn > widthOut ) gcd( widthIn, widthOut ) else gcd( widthOut, widthIn ) }
  val gcdType = UInt( (widthGcd*genWidth).W )
  val gcdWidthIn = widthIn/widthGcd
  val gcdWidthOut = widthOut/widthGcd
  val vecInComb = Wire( Vec( gcdWidthIn, gcdType.cloneType ) )
  val vecOutComb = Wire( Vec( gcdWidthOut, gcdType.cloneType ) )

  for( x <- vecInComb.toArray.zipWithIndex ) {
    val ary = new ArrayBuffer[T]()
    for ( idx <- 0 until widthGcd )
      ary += io.dataIn.bits( ( x._2 + 1 ) * widthGcd - idx - 1 )
    x._1 := ary.reduceLeft( (a, b) => ( a ## b ).asInstanceOf[T] )
  }
  for ( x <- vecOutComb.toArray.zipWithIndex ) {
    for ( idx <- 0 until widthGcd )
      io.dataOut.bits(x._2*widthGcd + idx) := x._1((idx + 1)*genWidth - 1, idx*genWidth)
  }

  val inBW = log2Up(gcdWidthIn)
  val outBW = log2Up(gcdWidthOut)

  // first trivial case is gcdWidthIn == gcdWidthOut
  if ( gcdWidthIn == gcdWidthOut ) {
    io.dataOut.valid := io.dataIn.valid
    io.dataOut.bits := io.dataIn.bits
    io.dataIn.ready := true.B
    io.flushed := io.flush
  }

  // second case is if gcdWidthIn < gcdWidthOut
  if ( gcdWidthIn < gcdWidthOut ) {
    // In the general case, we need to be able to wire each value to anywhere in the output
    // There will also be left over Input to be wrapped around
    val outPos = RegInit( 0.U( outBW.W ) ) // the position we are upto in the output reg
    val outPosNext = outPos + gcdWidthIn.U( outBW.W + 1 ) // the next position after this clock cycle
    val excess = outPos - (gcdWidthOut - gcdWidthIn).U( outBW.W ) // When the last of the output is filled in, how many of the input are left
    val remaining = gcdWidthOut.U( outBW.W + 1 ) - outPos // the number of spots remaining
    val filled = ( (gcdWidthOut - gcdWidthIn).U( outBW.W ) <= outPos ) && io.dataIn.valid // Indicates if the output was just filled
    val fillFlush = ( ( gcdWidthOut - gcdWidthIn ).U( outBW.W ) < outPos ) && io.dataIn.valid
    val flushReg = RegNext( fillFlush && io.flush )
    io.flushed := ( !fillFlush && io.flush ) || flushReg
    when ( io.flushed ) {
      flushReg := false.B
    }

    // at least 1 value can come directly from the input
    val tmpSig = Wire( Vec( gcdWidthOut - 1, gcdType ) )
    val tmpReg = Reg( tmpSig.cloneType )
    tmpReg := tmpSig
    tmpSig := tmpReg
    // a vec representing the mix from the input and register
    val tmpOut = Wire( Vec( gcdWidthIn, gcdType ) )

    // assign tmpOut from tmpReg and input
    for ( i <- 0 until gcdWidthIn ) {
      when ( excess > i.U( inBW.W ) ) {
        tmpOut(i) := tmpSig( ( gcdWidthOut - gcdWidthIn ).U( outBW.W ) + i.U( outBW.W ) )
        when ( filled ) {
          tmpOut(i) := tmpReg( ( gcdWidthOut - gcdWidthIn ).U( outBW.W ) + i.U( outBW.W ) )
        }
      } .otherwise {
        tmpOut(i) := vecInComb( i.U( inBW.W ) - excess )
      }
    }

    // assign the output
    for ( i <- 0 until ( gcdWidthOut - gcdWidthIn ) ) {
      vecOutComb(i) := tmpSig(i)
      when ( filled ) {
        vecOutComb(i) := tmpReg(i)
      }
    }
    for ( i <- 0 until gcdWidthIn ) {
      vecOutComb( i + gcdWidthOut - gcdWidthIn ) := tmpOut(i)
    }
    io.dataOut.valid := filled || ( io.flushed &&  (io.dataIn.valid ||  outPos =/= 0.U( outBW.W ) ) )
    io.dataIn.ready := !( fillFlush && io.flush ) || io.flushed

    // update the output position
    when ( io.dataIn.valid ) {
      outPos := outPosNext
      when ( filled ) {
        outPos := excess
      }
    }
    when ( io.flushed ) {
      outPos := 0.U( outBW.W )
    }

    val hiOut = Wire( UInt( outBW.W ) )
    val loOut = Wire( UInt( outBW.W ) )
    val hiIn = Wire( UInt( inBW.W ) )
    val loIn = Wire( UInt( inBW.W ) )

    when ( io.dataIn.valid && !( filled && ( excess === 0.U( outBW.W ) ) )) {
      DynamicVecAssign( tmpSig, hiOut, loOut, vecInComb, hiIn, loIn )
    }

    hiOut := outPosNext - 1.U( outBW.W )
    loOut := outPos
    hiIn := (gcdWidthIn - 1).U( inBW.W )
    loIn := 0.U( inBW.W )

    when ( filled ) {
      // excess > 0 for vec assign to be used
      hiOut := excess - 1.U( outBW.W )
      loOut := 0.U( outBW.W )
      loIn := remaining
    }
  }

  // final case if gcdWidthIn > gcdWidthOut
  if ( gcdWidthIn > gcdWidthOut ) {

    // Store the last gcdWidthOut - 1 values of width In
    val tmpRegWidth = { if ( gcdWidthOut == 1 ) 1 else gcdWidthOut - 1 }
    val vecRegType = Vec( tmpRegWidth, gcdType.cloneType )
    val tmpReg = Reg( vecRegType.cloneType )
    val inPos = RegInit( 0.U( inBW.W ) )
    val remaining = gcdWidthOut.U( inBW.W + 1 ) - inPos
    val inPosNext = inPos + gcdWidthOut.U( inBW.W + 1 )
    val leftOver = RegInit( false.B )
    val used = { ( !leftOver && ( ( gcdWidthIn - gcdWidthOut ).U( inBW.W + 1 ) < inPosNext ) && io.dataIn.valid ) ||
      leftOver && ( ( gcdWidthIn - gcdWidthOut ).U( inBW.W + 1 ) < inPos ) && io.dataIn.valid }
    val flushReg = RegInit( false.B )
    val flush = io.flush || flushReg
    flushReg := ( io.flush && io.dataIn.valid && ( inPosNext =/= gcdWidthIn.U( inBW.W + 1 ) ) ) || flushReg
    io.flushed := { ( io.dataIn.valid && flush && used && (inPosNext === gcdWidthIn.U( inBW.W + 1 ) ) ) ||
                    ( io.dataIn.valid && flushReg && leftOver ) ||
                    ( !io.dataIn.valid && io.flush ) }

    when ( io.dataIn.valid ) {
      for ( i <- 0 until (gcdWidthOut - 1) ) {
        tmpReg(i) := vecInComb( gcdWidthIn - gcdWidthOut + 1 + i )
      }
      inPos := inPosNext
    }
    val tmpOut = Wire( Vec( gcdWidthOut, gcdType.cloneType ) )
    for ( i <- 0 until gcdWidthOut ) {
      val tmpRegIdx = (inPos - 1.U( inBW.W )) + i.U( outBW.W )
      when ( i.U( outBW.W ) < remaining ) {
        tmpOut(i) := tmpReg( tmpRegIdx( outBW - 1, 0 ) ) // because Width doesn't have '-' for some reason ...
      } .otherwise {
        tmpOut(i) := vecInComb( i.U( inBW.W ) - remaining )
      }
    }
    when ( leftOver && io.dataIn.valid) {
      leftOver := false.B
      inPos := inPos
    }
    when ( used ) {
      inPos := 0.U( inBW.W )
      when( inPosNext =/= gcdWidthIn.U( inBW.W + 1 ) ) {
        leftOver := true.B
        inPos := inPosNext - ( gcdWidthIn - gcdWidthOut ).U( inBW.W + 1 )
        when ( leftOver ) {
          inPos := inPosNext - gcdWidthIn.U( inBW.W + 1 )
        }
      }
    }

    when ( io.flushed ) {
      inPos := 0.U( inBW.W )
      leftOver := false.B
      flushReg := false.B
    }

    io.dataIn.ready := ( used && !flush ) || io.flushed
    io.dataOut.valid := io.dataIn.valid || ( io.flushed && leftOver )
    // dynVec with default value
    val dynVecOut = Wire( Vec( gcdWidthOut, gcdType.cloneType ) )
    for ( i <- 0 until gcdWidthOut )
      dynVecOut( i ) := tmpOut( i )

    DynamicVecAssign( dynVecOut, ( gcdWidthOut - 1 ).U( outBW.W ), 0.U( outBW.W ), vecInComb, inPosNext - 1.U( inBW.W ), inPos )
    vecOutComb := dynVecOut
    when ( leftOver ) {
      vecOutComb := tmpOut
    }
  }
}
