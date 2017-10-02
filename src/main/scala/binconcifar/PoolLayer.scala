
package binconcifar

import chisel3._
import chisel3.util._

/** A Max Pool something
  */
private class MaxPool[T <: Bits with Num[T]]( dtype : T, kernShape : ( Int, Int, Int ) ) extends Module {

  val io = IO( new Bundle {
    val dataIn = Input(Vec( kernShape._1, Vec( kernShape._2, Vec( kernShape._3, dtype.cloneType ))))
    val dataOut = Output(Vec( kernShape._3, dtype.cloneType ))
  })

  def getMax( numsToCmp : Seq[T] ) : ( T, Int ) = {
    var maxedNums = numsToCmp.toList
    var stages = 0
    assert( maxedNums.size >= 1, "Must be atleast one number to find max of" )
    while ( maxedNums.size > 1 ) {
      val toCmp = maxedNums.grouped(2).toList
      maxedNums = toCmp.map( grp => {
        if ( grp.size == 1 )
          RegNext( grp(0) )
        else {
          val maxVal = Reg( dtype.cloneType.cloneType )
          maxVal := grp(0)
          when ( grp(1) >= grp(0) ) {
            maxVal := grp(1)
          }
          maxVal
        }
      })
      stages += 1
    }
    ( maxedNums.head, stages )
  }

  // for each collection, compute the max
  val paraOuts = ( 0 until kernShape._3 ).map( idx => {
    val numsToCmp = io.dataIn.reduce( (a, b) => Vec( a ++ b ) ).map( x => x(idx) )
    getMax( numsToCmp )
  })
  
  val latency = paraOuts.map( _._2 ).max

  io.dataOut := Vec( paraOuts.map( po => {
    ShiftRegister( po._1, latency - po._2 )
  }))

}

class PoolLayer( tput : Double, val kernShape : (Int, Int, Int) )
    extends NNLayer( tput, kernShape._1 * kernShape._2 * kernShape._3,
      kernShape._3, tput.toInt ) {

  io.dataIn.ready := io.dataOut.ready

  var tmpLat = -1

  if ( throughput >= 1 ) {
    val ioSplit = inIOToVVV( kernShape._2, kernShape._3 )
    // create the full throughput MP Blocks
    val fullTPut = ( 0 until throughput.toInt ).map( idx => {
      val mp = Module( new MaxPool( dtype, kernShape ) )
      mp.io.dataIn := Vec( ( 0 until kernShape._1 ).map( k => {
        ioSplit( k + idx * kernShape._1 )
      }))
      ( mp.io.dataOut, mp.latency )
    })
    tmpLat = fullTPut.map( _._2 ).max
    val cycMatch = fullTPut.map( res => {
      ShiftRegister( res._1, tmpLat - res._2 )
    })
    io.dataOut.bits := cycMatch.reduce( (a, b) => Vec(a ++ b) )
  }

  val latency = tmpLat

  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency, false.B, true.B )
}
