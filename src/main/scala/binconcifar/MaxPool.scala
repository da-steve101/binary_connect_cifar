
package binconcifar

import chisel3._
import chisel3.util._

class MaxPool( tput : Double, kernShape : (Int, Int, Int) ) extends TriConvCompute( tput,
  ( kernShape._1, kernShape._2, kernShape._3, kernShape._1 ) ) {

  io.dataIn.ready := true.B

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
          val maxVal = Reg( dtype.cloneType )
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
  val paraOuts = io.dataIn.bits.toSeq.map( poolCol => {
    ( 0 until kernShape._1 ).map( idx => {
      val numsToCmp = poolCol.toSeq.map( _.toSeq ).reduce( _ ++ _ ).map( x => x(idx) )
      getMax( numsToCmp )
    })
  }).reduce( _ ++ _ )

  val latency = paraOuts.map( _._2 ).max

  io.dataOut.bits := paraOuts.map( po => {
    ShiftRegister( po._1, latency - po._2 )
  })

  io.dataOut.valid := ShiftRegister( io.dataIn.valid, latency )
}
