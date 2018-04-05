
package binconcifartests

import chisel3._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import scala.util.Random
import binconcifar.SparseMatMul
import binconcifar.SparseMatMulSerial
import binconcifar.TriConvSum
import scala.collection.mutable.ArrayBuffer

class ComparisonModule extends Module {

  val idx = 2
  val inSize = 3*3* {
    if ( idx == 1 )
      3
    else if ( idx == 2 || idx == 3 )
      64
    else if ( idx == 4 || idx == 5 )
      128
    else
      256
  }
  val outSize = {
    if ( idx == 1 || idx == 2 )
      64
    else if ( idx == 3 || idx == 4 )
      128
    else
      256
  }
  val dtype = SInt( 16.W )
  val outFormat = ( 3, 3, 3 )
  val tPutLyr = 0.25
  val fanoutReg = 1

  val io = IO( new Bundle {
    val dataIn = Flipped( Valid( Vec( inSize, dtype ) ) )
    val dataOut_s = Valid( Vec( outSize, dtype ) )
    val dataOut_orig = Valid( Vec( outSize, dtype ) )
  })

  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/cifar_layer" + idx + "_tern_op_list.csv" )
  // val bufferedSource = scala.io.Source.fromFile("src/main/resources/cifar_layer" + idx + "_op_list.csv" )
  val bufferedSource = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_tern_op_list.csv" )
  val data_src = bufferedSource.getLines.toList
  val data_ints = data_src.map( _.split(",").toList.map( x => {
    x.toInt
  }))

  val outputIdxs = data_ints.head.toList
  val treeDefinition = data_ints.tail.toList

  val bufferedSource_2 = scala.io.Source.fromFile("src/main/resources/conv" + idx + "_weights.csv" )
  val weights_raw = bufferedSource_2.getLines.toList
  val weights = weights_raw.map( _.split(",").toList.map( x => {
    x.toInt
  }) ).grouped( outFormat._3 ).toList.grouped( outFormat._2 ).toList
  val weights_trans = ( 0 until outSize ).toList.map( i =>
    weights.map( w0 => w0.map( w1 => w1.map( w2 => w2(i) ) ) )
  ).map( x => x.map( _.reverse ).reverse )

  val conv_s = Module( new SparseMatMulSerial( dtype, treeDefinition, outputIdxs, 4 ) )

  val conv_orig = Module( new TriConvSum( dtype, weights_trans, tPutLyr, fanoutReg ) )

  // reorder bits in to match triconvsum
  val modOrder = Vec( io.dataIn.bits.grouped(3).toList.reverse.reduce( _ ++ _ ) )
  conv_s.io.dataIn.bits := modOrder
  conv_s.io.dataIn.valid := io.dataIn.valid
  conv_orig.io.dataIn.bits := io.dataIn.bits
  conv_orig.io.dataIn.valid := io.dataIn.valid

  io.dataOut_s.bits := Vec( conv_s.io.dataOut.bits.take( outSize ) )
  io.dataOut_s.valid := conv_s.io.dataOut.valid
  conv_s.io.dataOut.ready := true.B
  io.dataOut_orig.bits := Vec( conv_orig.io.dataOut.bits.take( outSize ) )
  io.dataOut_orig.valid := conv_orig.io.dataOut.valid
  conv_orig.io.dataOut.ready := true.B

  val latency = 10
}

class ComparisonTests( c : ComparisonModule ) extends PeekPokeTester( c ) {

  val myRand = new Random
  val cycs = 10

  def getRndFP() : BigInt = {
    val x = 2 * myRand.nextDouble() - 1
    BigInt( math.round( x * ( 1 << 3 ) ).toInt )
  }


  val img = ( 0 until cycs ).toList.map( x => {
    List.fill( c.inSize ) { getRndFP() }
  })

  val sparse_res = ArrayBuffer[List[BigInt]]()
  val orig_res = ArrayBuffer[List[BigInt]]()

  poke( c.io.dataIn.valid, true )
  for ( cyc <- 0 until cycs ) {
    val imgData = img(cyc)
    println( "imgData = " + imgData )
    for ( dataIn1 <- c.io.dataIn.bits.zip( imgData ) )
      poke( dataIn1._1, dataIn1._2 )

    val data_s_vld = peek( c.io.dataOut_s.valid ) == 1
    if ( data_s_vld ) {
      if ( sparse_res.size < orig_res.size ) {
        for ( i <- 0 until c.outSize )
          expect( c.io.dataOut_s.bits(i), orig_res( sparse_res.size )( i ) )
      }
      val dataOut_s = ( 0 until c.outSize ).map( i => peek( c.io.dataOut_s.bits( i ) ) )
      sparse_res += dataOut_s.toList
    }
    val data_orig_vld = peek( c.io.dataOut_orig.valid ) == 1
    if ( data_orig_vld ) {
      if ( sparse_res.size > orig_res.size ) {
        for ( i <- 0 until c.outSize )
          expect( c.io.dataOut_orig.bits(i), sparse_res( orig_res.size )( i ) )
      }
      val dataOut_orig = ( 0 until c.outSize ).map( i => peek( c.io.dataOut_orig.bits( i ) ) )
      orig_res += dataOut_orig.toList
    }
    step(1)
  }
}

class ComparisonSuite extends ChiselFlatSpec {

  behavior of "ComparisonSuite"
  backends foreach {backend =>
    it should s"correctly compute the convolution $backend" in {
      Driver(() => { new ComparisonModule  }, "verilator", true )(
        c => new ComparisonTests( c ) ) should be (true)
    }
  }
}
