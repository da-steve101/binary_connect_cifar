
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.PoolLayer
import scala.collection.mutable.ArrayBuffer

class PoolComputeTests( c : PoolLayer[SInt] ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = c.latency*5

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 4 ) ).toInt )
  }

  val img = List.fill( cycs ) {
    List.fill( c.noIn ) {
      List.fill( c.kernShape._1 ) {
        List.fill( c.kernShape._2 ){
          List.fill( c.kernShape._3 ) { getRndFP() }
        }
      }
    }
  }

  val poolRes = img.map( poolTasks => {
    poolTasks.map( poolTask => {
      ( 0 until c.kernShape._3 ).map( idx => {
        poolTask.reduce( _ ++ _ ).map( _(idx) ).max
      })
    }).reduce( _ ++ _ )
  })

  val chosenOutput = ArrayBuffer[List[BigInt]]()
  val vldCheck = ArrayBuffer[Boolean]()
  var inputPtr = 0
  var outputPtr = 0
  for ( cyc <- 0 until cycs ) {
    val vld = myRand.nextInt(4) != 0
    vldCheck += vld
    poke( c.io.dataIn.valid, vld )
    val imgData = img(inputPtr).reduce( _ ++ _ ).reduce( _ ++ _ ).reduce( _ ++ _ )
    for ( dataIn1 <- c.io.dataIn.bits.zip( imgData ) )
      poke( dataIn1._1, dataIn1._2 )
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
  backends foreach {backend =>
    it should s"correctly compute the max pool $backend" in {
      for ( grpSize <- List( 1, 3, 8 ) ) {
        for ( tPut <- 1 until 6 ) {
          val kernShape = ( 2, 2, grpSize )
          Driver(() => {
            new PoolLayer( SInt( 16.W ), tPut, kernShape )
          }, backend, true )( c => new PoolComputeTests( c ) ) should be (true)
        }
      }
    }
  }
}
