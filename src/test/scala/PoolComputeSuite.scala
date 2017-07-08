
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.{TriConvCompute, MaxPool}
import scala.collection.mutable.ArrayBuffer

class PoolComputeTests( c : TriConvCompute, kernShape : (Int, Int, Int) )
    extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = c.latency*3

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 4 ) ).toInt )
  }

  val img = List.fill( cycs ) {
    List.fill( kernShape._3 ) {
      List.fill( kernShape._2 ){
        List.fill( kernShape._1 ) { getRndFP() }
      }
    }
  }

  val poolRes = img.map( poolTask => {
    ( 0 until kernShape._1 ).map( idx => {
      poolTask.reduce( _ ++ _ ).map( _(idx) ).max
    })
  })

  val chosenOutput = ArrayBuffer[List[BigInt]]()
  val vldCheck = ArrayBuffer[Boolean]()
  var inputPtr = 0
  var outputPtr = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(2) != 0
    vldCheck += vld
    poke( c.io.dataIn.valid, vld )
    for ( dataIn1 <- c.io.dataIn.bits(0).zip( img(inputPtr) ) ) {
      for ( dataIn2 <- dataIn1._1.zip( dataIn1._2 ) ) {
        for ( i <- 0 until dataIn2._2.size )
          poke( dataIn2._1(i), dataIn2._2(i) )
      }
    }
    if ( vld )
      chosenOutput += poolRes(inputPtr).toList
    inputPtr += 1
    step( 1 )
    if ( cyc >= c.latency - 1 ) {
      expect( c.io.dataOut.valid, vldCheck( cyc - c.latency + 1 ) )
      if ( vldCheck( cyc - c.latency + 1 ) ) {
        for ( i <- 0 until chosenOutput( outputPtr ).size )
          expect( c.io.dataOut.bits(i), chosenOutput( outputPtr )(i) )
        outputPtr += 1
      }
    }
  }
}

class PoolComputeSuite extends ChiselFlatSpec {
  behavior of "PoolCompute"
  val kernShape = ( 4, 2, 2 )
  backends foreach {backend =>
    it should s"correctly compute the max pool $backend" in {
      Driver(() => {
        val c : TriConvCompute = new MaxPool( 1, kernShape )
        c
      }, backend )( c => new PoolComputeTests( c, kernShape  ) ) should be (true)
    }
  }
}
