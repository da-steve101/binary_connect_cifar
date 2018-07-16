
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object Fanout {

  def apply[ T <: Bits ](
    tmp : Vec[T],
    stages : Int,
    noOut : Int
  ) : List[Vec[T]] = {
    val delayActs = tmp.map( x => {
      val fanMod = Module( new Fanout( x, stages, noOut ) )
      fanMod.io.in := x
      fanMod.io.out
    }).toList
    delayActs
  }

  def apply[ T <: Bits ] (
    tmp : Vec[T],
    stages : Int,
    noOut : List[Int]
  ) : List[Vec[T]] = {
    tmp.zip( noOut ).map( x => {
      val fanMod = Module( new Fanout( x._1, stages, x._2 ) )
      fanMod.io.in := x._1
      fanMod.io.out
    }).toList
  }
}

class Fanout[ T <: Bits ](
  gentype : T,
  val stages : Int,
  noOut : Int,
  reset : Boolean = false
) extends Module {

  Predef.assert( stages > 0, "must be atleast one stage" )

  val io = IO( new Bundle {
    val in = Input( gentype.cloneType )
    val out = Output( Vec( noOut, gentype.cloneType ) )
  })

  def calcNumInStage() : List[Int] = {
    // calc the ideal number of fanout in each stage
    val perStage = math.exp( math.log( noOut ) / stages )
    val numInStage = ArrayBuffer[Int]()
    for ( i <- 1 until stages )
      numInStage.append( math.round( math.pow( perStage, i ).toFloat ) )
    numInStage.append( noOut )
    numInStage.toList
  }

  val numInStage = calcNumInStage()

  val dataVecs = ArrayBuffer[Vec[T]]()
  val inVec = Wire( Vec( 1, gentype.cloneType ) )
  inVec(0) := io.in
  dataVecs.append( inVec )
  val zero = 0.U.asTypeOf( gentype )
  for ( s <- numInStage.zipWithIndex ) {
    val tmpVec = Wire( Vec( s._1, gentype.cloneType ) )
    var sCnt = 0
    for ( i <- 0 until s._1 ) {
      if ( reset ) {
        val r = RegInit( zero )
        r := dataVecs( s._2 )( sCnt )
        tmpVec( i ) := r
      } else
        tmpVec( i ) := RegNext( dataVecs( s._2 )( sCnt ) )
      sCnt = ( sCnt + 1 ) % dataVecs( s._2 ).size
    }
    dataVecs.append( tmpVec )
  }

  for ( i <- 0 until noOut )
    io.out(i) := dataVecs.last( i )

}
