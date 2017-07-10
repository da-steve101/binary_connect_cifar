
package binconcifartests

import chisel3._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.DynamicVecAssign
import scala.collection.mutable.ArrayBuffer

class UserMod( val vecInSize : Int, val vecOutSize : Int) extends Module {
  val io = IO(new Bundle {
    val vecIn = Input( Vec( vecInSize, UInt( 4.W ) ) )
    val hiIn = Input( UInt( log2Up(vecInSize).W ) )
    val loIn = Input( UInt( log2Up(vecInSize).W ) )
    val hiOut = Input( UInt( log2Up(vecOutSize).W ) )
    val loOut = Input( UInt( log2Up(vecOutSize).W ) )
    val vecOut = Output( Vec( vecOutSize, UInt( 4.W ) ) )
  })
  for ( i <- 0 until vecOutSize )
    io.vecOut(i) := 0.U( 4.W )
  DynamicVecAssign(io.vecOut, io.hiOut, io.loOut, io.vecIn, io.hiIn, io.loIn)
}

class DynamicVecAssignTests( c : UserMod ) extends PeekPokeTester( c ) {

  val myRand = new Random
  val vecLen = myRand.nextInt( scala.math.min( c.vecInSize, c.vecOutSize ) ) + 1
  val hiIn = myRand.nextInt( c.vecInSize + 1 - vecLen ) + vecLen - 1
  val loIn = hiIn + 1 - vecLen
  val hiOut = myRand.nextInt( c.vecOutSize + 1 - vecLen ) + vecLen - 1
  val loOut = hiOut + 1 - vecLen
  val vecIn = ArrayBuffer.fill( c.vecInSize ) { myRand.nextInt(16) }

  vecIn.zipWithIndex.map( x => poke(c.io.vecIn(x._2), x._1) )
  poke(c.io.hiIn, hiIn)
  poke(c.io.loIn, loIn)
  poke(c.io.hiOut, hiOut)
  poke(c.io.loOut, loOut)
  for( x <- 0 until vecLen )
    expect( c.io.vecOut(x + loOut), vecIn(x + loIn ) )
}


class DynamicVecAssignSuite extends ChiselFlatSpec {
  behavior of "DynamicVecAssign"
  backends foreach {backend =>
    it should s"correctly and dynamically assign values $backend" in {
      for ( vecOutSize <- 1 until 20 ) {
        for ( vecInSize <- 1 until 20 ) {
          Driver(() => {
            new UserMod( vecInSize, vecOutSize )
          }, backend )( c => new DynamicVecAssignTests( c ) ) should be (true)
        }
      }
    }
  }
}
