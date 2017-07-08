
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.{TriConvCompute, ParallelTriConvSum}
import scala.collection.mutable.ArrayBuffer

class ConvSumTests( c : TriConvCompute, weights : List[List[List[List[Int]]]] )
 extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = c.latency*3

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 4 ) ).toInt )
  }

  val img = List.fill( cycs ) {
    List.fill( weights(0).size ) {
      List.fill( weights(0)(0).size ){
        List.fill( weights(0)(0)(0).size ) { getRndFP() }
      }
    }
  }

  val convRes = img.map( imgFrame => {
    weights.map( convFrame => {
      convFrame.zip( imgFrame ).map( ci1 => {
        ci1._1.zip( ci1._2 ).map( ci2 => {
          ci2._1.zip( ci2._2 ).map( ci3 => {
            if ( ci3._1 == 1 )
              ci3._2
            else if ( ci3._1 == -1 )
              -ci3._2
            else
              BigInt( 0 )
          }).sum
        }).sum
      }).sum
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
    for ( dataIn1 <- c.io.dataIn.bits(0).zip( img(cyc) ) ) {
      for ( dataIn2 <- dataIn1._1.zip( dataIn1._2 ) ) {
        for ( i <- 0 until dataIn2._2.size )
          poke( dataIn2._1(i), dataIn2._2(i) )
      }
    }
    if ( vld )
      chosenOutput += convRes(cyc).toList
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

class ConvSumSuite extends ChiselFlatSpec {
  behavior of "ConvSum"
  val weights = List(
    List(
      List( List( 1, 0, 1 ), List( -1, 0, 0 ), List( -1, 1, 0 ) ),
      List( List( 0, 0, -1 ), List( 1, 0, 0 ), List( -1, -1, 1 ) ),
      List( List( 1, 0, 0 ), List( 0, 0, 0 ), List( 1, 0, 0 ) )
    ),
    List(
      List( List( 1, 0, 1 ), List( -1, -1, 0 ), List( 0, 1, 0 ) ),
      List( List( -1, 0, 0 ), List( -1, 0, 1 ), List( -1, 0, -1 ) ),
      List( List( 0, 1, -1 ), List( 0, -1, 0 ), List( -1, 0, 0 ) )
    ),
    List(
      List( List( 1, 0, 0 ), List( 1, 0, -1 ), List( 1, 1, 0 ) ),
      List( List( 0, 1, 1 ), List( 0, 1, 0 ), List( 1, 1, -1 ) ),
      List( List( -1, 0, -1 ), List( 1, -1, 0 ), List( 0, 1, 0 ) )
    ),
    List(
      List( List( 0, 1, -1 ), List( 0, 0, 1 ), List( 0, 1, -1 ) ),
      List( List( -1, 0, 0 ), List( -1, 0, -1 ), List( 0, 1, -1 ) ),
      List( List( 1, 0, 1 ), List( -1, 1, 1 ), List( -1, 0, -1 ) )
    )
  )
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => {
        val c : TriConvCompute = new ParallelTriConvSum( weights, 1 )
        c
      }, backend )( c => new ConvSumTests(c, weights) ) should be (true)
    }
  }
}
