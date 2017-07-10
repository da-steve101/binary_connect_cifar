

package binconcifartests

import chisel3._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.Serializer
import scala.collection.mutable.ArrayBuffer

class SerUserMod( val vecInSize : Int, val vecOutSize : Int, val bitWidth : Int) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped( Decoupled( Vec( vecInSize, UInt( bitWidth.W ) ) ) )
    val flush = Input( Bool() )
    val dataOut = Output( Valid( Vec( vecOutSize, UInt( bitWidth.W ) ) ) )
    val flushed = Output( Bool() )
  })
  val genType = Wire( UInt( bitWidth.W ) )
  val serMod = Serializer( genType, vecInSize, vecOutSize)
  io <> serMod.io
}

class SerializerTests( c : SerUserMod, cycles : Int ) extends PeekPokeTester(c) {

  println( "bitWidth = " + c.bitWidth )
  println( "vecInSize = " + c.vecInSize )
  println( "vecOutSize = " + c.vecOutSize )

  val myRand = new Random
  val inputData = ArrayBuffer.fill( cycles ) { ArrayBuffer.fill( c.vecInSize ) { myRand.nextInt( 1 << c.bitWidth ) } }
  var count = 0;
  var countOld = count
  var lastVld = false
  var outCount = 0;
  var flushOutCount = count*c.vecInSize

  while ( count < cycles ) {
    val inputVld = if ( count != countOld || !lastVld) (myRand.nextInt(5) != 0) else lastVld
    lastVld = inputVld
    val flush = (myRand.nextInt(15) == 0)
    if ( flush ) {
      flushOutCount = { if (inputVld) (count + 1)*c.vecInSize else count*c.vecInSize }
      // println("flushOutCount = " + flushOutCount)
    }
    poke( c.io.flush, flush )
    poke( c.io.dataIn.valid, inputVld )
      (0 until c.vecInSize).foreach( i => poke( c.io.dataIn.bits(i), inputData(count)(i) ) )
    val outputValid = peek( c.io.dataOut.valid )
    val flushed = ( peek( c.io.flushed ) == BigInt(1) )
    if ( outputValid == 1) {
      for ( i <- 0 until c.vecOutSize ) {
        // println("outCount = " + outCount)
        val outCyc = (outCount - (outCount % c.vecInSize)) / c.vecInSize
        if ( ( flushed && ( outCount < flushOutCount ) ) || !flushed ) {
          expect( c.io.dataOut.bits(i), inputData( outCyc )( outCount - (outCyc * c.vecInSize )) )
          outCount = outCount + 1
        } else
          peek( c.io.dataOut.bits(i) )
      }
    }
    val ready = peek(c.io.dataIn.ready)
    countOld = count
    if ( ready == 1 && inputVld ) {
      count = count + 1
    }
    step(1)
  }
}

class SerializerSuite extends ChiselFlatSpec {
  behavior of "Serializer"
  val myRand = new Random
  val bwToTry = List.fill( 3 ) { myRand.nextInt( 15 ) + 1 }
  val vecOutToTry = List.fill( 3 ) { myRand.nextInt( 20 ) + 1 }
  val vecInToTry = List.fill( 3 ) { myRand.nextInt( 20 ) + 1 }
  backends foreach {backend =>
    it should s"change the rate at which data flows $backend" in {
      for ( bitWidth <- bwToTry ) {
        for ( vecOutSize <- vecOutToTry ) {
          for ( vecInSize <- vecInToTry ) {
            val noCyc = scala.math.max( vecOutSize, vecInSize )*5
            Driver(() => {
              new SerUserMod( vecInSize, vecOutSize, bitWidth )
            }, backend )( c => new SerializerTests( c, noCyc ) ) should be (true)
          }
        }
      }
    }
  }
}
