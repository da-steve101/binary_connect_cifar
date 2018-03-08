package binconcifartests

import chisel3._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.SparseMatMul
import scala.collection.mutable.ArrayBuffer

class SparseMatMulTests( c : SparseMatMul ) extends PeekPokeTester( c ) {
  val myRand = new Random
  val cycs = c.latency*20

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 3 ) ).toInt )
  }

  val img = List.fill( cycs ) {
    List.fill( c.noInputs ) { getRndFP() }
  }

  val convRes = img.map( imgFrames => {
    val treeNodes = ArrayBuffer.fill( c.treeNodes.size ) { BigInt(0) }
    for ( d <- imgFrames.zipWithIndex )
      treeNodes( d._2 ) = d._1
    for ( op <- c.treeDefinition ) {
      if ( op(2) < 0 ) {
        if ( op(1) < 0 )
          treeNodes( op(0) ) = 0
        else {
          if ( op( 3 ) == 1 )
            treeNodes( op(0) ) = treeNodes( op(1) )
          else
            treeNodes( op(0) ) = -treeNodes( op(1) )
        }
      } else {
        if ( op( 3 ) == 1 )
          treeNodes( op(0) ) = treeNodes( op(1) ) + treeNodes( op(2) )
        else if ( op( 3 ) == 0 )
          treeNodes( op(0) ) = treeNodes( op(1) ) - treeNodes( op(2) )
        else
          treeNodes( op(0) ) = - treeNodes( op(1) ) - treeNodes( op(2) )
      }
    }
    c.outputIdxs.map( i => {
      if ( i < 0 )
        BigInt( 0 )
      else
        treeNodes( i )
    })
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
    val imgData = img(inputPtr)
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

class SparseMatMulSuite extends ChiselFlatSpec {

  /*
  // inputs 0-3
  val treeDefinition = List(
    List( 4, 0, 1, 1 ),
    List( 5, 2, 3, 1 ),
    List( 6, 0, 2, 1 ),
    List( 7, 0, 3, 0 ),
    List( 8, 1, 3, 0 ),
    List( 9, 2, 3, 1 ),
    List( 10, 3, -1, 1 ),
    List( 11, 0, -1, 0 ),
    List( 12, 2, -1, 1 ),
    List( 13, 1, 2, 1 ),
    List( 14, 1, -1, 1 ),
    List( 15, 12, 13, 0 ),
    List( 16, 4, 10, 1 ),
    List( 17, 5, 14, 1 ),
    List( 18, 6, 11, 1 ),
    List( 19, 8, 9, 0),
    List( 20, 7, -1, 1 ),
    List( 21, 9, -1, 0 ),
    List( 22, 13, -1, 1 )
  )

  val outputIdxs = List( 15, 16, 17, 18, 19, 20, 21, 22 )
   */
  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/conv2_weights_op_list.csv")
  val bufferedSource = scala.io.Source.fromFile("src/main/resources/cifar_layer1_op_list.csv")
  val data_src = bufferedSource.getLines.toList
  val data_ints = data_src.map( _.split(",").toList.map( x => {
    x.toInt
  }))

  val outputIdxs = data_ints.head.toList
  val treeDefinition = data_ints.tail.toList

  behavior of "SparseMatMulSuite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => {
        new SparseMatMul( SInt( 16.W ), treeDefinition, outputIdxs )
      }, "verilator", true )( c => new SparseMatMulTests( c ) ) should be (true)
    }
  }
}
