package binconcifar

import chisel3._
import chisel3.util._
import java.io._

class SSILayer[T <: Bits]( val dtype : T, val tPutIn : Int, val tPutOut : Int ) extends Module {

  val io = IO( new Bundle {
    val dataIn = Flipped( Decoupled( Vec( tPutIn, dtype ) ) )
    val dataOut = Decoupled( Vec( tPutOut, dtype ) )
  })

  val ratioIn : Int = ( tPutIn/tPutOut )
  val ratioOut : Int = ( tPutOut/tPutIn )
  val bufLen : Int = scala.math.max( tPutIn, tPutOut )

  val buffer = {
    if ( tPutIn >= tPutOut )
      Reg( io.dataIn.bits.cloneType )
    else
      Reg( io.dataOut.bits.cloneType )
  }
  if ( tPutIn >= tPutOut ) {
    for ( i <- 1 until ratioIn ) {
      for ( j <- 0 until tPutOut )
        // buffer(i*tPutOut + j ) := buffer((i-1)*tPutOut + j )
        buffer((i-1)*tPutOut + j ) := buffer(i*tPutOut + j )
    }
  } else {
    for ( i <- 1 until ratioOut ) {
      for ( j <- 0 until tPutIn )
        // buffer(i*tPutIn + j ) := buffer((i-1)*tPutIn + j )
        buffer((i-1)*tPutIn + j ) := buffer(i*tPutIn + j )
    }
  }
  when ( io.dataIn.valid ) {
    for ( i <- 0 until tPutIn )
      // buffer( i) := io.dataIn.bits(i)
      buffer( bufLen - tPutIn + i ) := io.dataIn.bits(i)
  }
  if ( tPutIn >= tPutOut ) {
    val srvld = RegInit( 0.U( ratioIn.W ) )
    srvld := srvld << 1
    when ( io.dataIn.valid ) {
      srvld := ( ( BigInt(1) << ratioIn ) - 1 ).U
    }
    io.dataOut.valid := srvld( ratioIn - 1 )
  } else {
    val srvld = RegInit( 0.U( ratioOut.W ) )
    srvld := srvld << 1
    when ( io.dataIn.valid & ( srvld( ratioOut - 2, 0 ) === 0.U ) ) {
      srvld := 1.U( ratioOut.W )
    }
    io.dataOut.valid := srvld( ratioOut - 1 )
  }

  io.dataIn.ready := true.B
  for ( i <- 0 until tPutOut )
    // io.dataOut.bits(tPutOut - i - 1) := buffer( bufLen - i - 1 )
    io.dataOut.bits( i ) := buffer( i )
}
