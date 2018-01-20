
package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.TriConvSum
import scala.collection.mutable.ArrayBuffer

class ConvSumTests( c : TriConvSum ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = c.latency*20

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 3 ) ).toInt )
  }

  val img = List.fill( cycs ) {
    List.fill( c.noIn ) {
      List.fill( c.weights(0).size ) {
        List.fill( c.weights(0)(0).size ){
          List.fill( c.weights(0)(0)(0).size ) { getRndFP() }
        }
      }
    }
  }

  val convRes = img.map( imgFrames => {
    imgFrames.map( imgFrame => {
      c.weights.map( convFrame => {
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
    }).reduce( _ ++ _ )
  })

  val chosenOutput = ArrayBuffer[List[BigInt]]()
  var inputPtr = 0
  var outputPtr = 0
  var vld = false
  poke( c.io.dataOut.ready, true )
  poke( c.io.dataIn.valid, vld )
  for ( cyc <- 0 until cycs ) {
    vld = vld ||  myRand.nextInt(4) != 0
    poke( c.io.dataIn.valid, vld )
    val rdy = peek( c.io.dataIn.ready ) == 1
    val imgData = img(inputPtr).reduce( _ ++ _ ).reduce( _ ++ _ ).reduce( _ ++ _ )
    for ( dataIn1 <- c.io.dataIn.bits.zip( imgData ) )
      poke( dataIn1._1, dataIn1._2 )
    if ( vld && rdy ) {
      chosenOutput += convRes(inputPtr).toList
      inputPtr += 1
    }
    if ( rdy )
      vld = false
    val vldOut = peek( c.io.dataOut.valid ) == 1
    peek( c.io.dataOut.bits )
    if ( vldOut ) {
      for ( i <- 0 until chosenOutput( outputPtr ).size )
        expect( c.io.dataOut.bits(i), chosenOutput( outputPtr )(i) )
      outputPtr += 1
    }
    step( 1 )
  }
}

class ConvSumSuite extends ChiselFlatSpec {

  val myRand = new Random
  def generate_filter( outFormat : ( Int, Int, Int ) ) : List[List[List[Int]]] = {
    ( 0 until outFormat._1 ).map( i => {
      ( 0 until outFormat._2 ).map( j => {
        ( 0 until outFormat._3 ).map( k => {
          myRand.nextInt( 3 ) - 1
        }).toList
      }).toList
    }).toList
  }
  val tPut = 0.25
  val filter_size = 3
  val grpSize = 8

  behavior of "ConvSumSuite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      val outFormat = ( filter_size, filter_size, grpSize )
      val weights = List.fill( 8 ) { generate_filter( outFormat ) }
      println( "outFormat = " + outFormat + ", tPut " + tPut )
      Driver(() => {
        new TriConvSum( SInt( 16.W ), weights, tPut )
      }, "verilator", false )( c => new ConvSumTests( c ) ) should be (true)
    }
  }
}
