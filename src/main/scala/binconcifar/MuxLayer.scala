
package binconcifar

import chisel3._
import chisel3.util._
import collection.mutable.ArrayBuffer

class MuxLayer( dtype : SInt, val inSize : Int, val outSize : Int ) extends Module {
  val io = IO( new Bundle {
    val dataIn = Flipped( Decoupled( Vec( inSize, dtype ) ) )
    val dataOut = Decoupled( Vec( outSize, dtype ) )
  })

  io.dataIn.ready := io.dataOut.ready
  val noGrps = inSize / outSize
  val grpsBits = log2Ceil( noGrps )
  val cntr = RegInit( 0.U( grpsBits.W ) )
  val data = (0 until noGrps).map( os => Reg( Vec( outSize, dtype ) ) ).toList
  when ( cntr > 0.U ) {
    cntr := cntr + 1.U
  }
  when ( io.dataIn.valid && cntr === 0.U ) {
    cntr := 1.U
    for ( i <- 0 until noGrps ) {
      for ( j <- 0 until outSize )
        data( i )( j ) := io.dataIn.bits( i*outSize + j )
    }
  }

  val bitsPerCycle = 2
  val selPerCycle = 1 << bitsPerCycle

  val buffers = ArrayBuffer[List[Vec[SInt]]]()
  buffers += data
  val cycles = math.ceil( grpsBits / bitsPerCycle ).toInt
  val cntrs = ( 0 until cycles ).map( cycle => {
    val highBit = bitsPerCycle*( cycle + 1 ) - 1
    val lowBit = bitsPerCycle*cycle
    val tmp = cntr( highBit, lowBit )
    ShiftRegister( tmp, cycle + 1 )
  }).toList
  for ( cycle <- 0 until cycles ) {
    val cntrBits = cntrs( cycle )
    val switchData = buffers.last.grouped( selPerCycle ).toList
    val newData : List[Vec[SInt]] = switchData.map( grp => {
      val selData = Wire( Vec( outSize, dtype ) )
      selData := grp(0)
      for ( i <- 1 until selPerCycle ) {
        when ( cntrBits === i.U ) {
          selData := grp(i)
        }
      }
      RegNext( selData )
    })
    buffers += newData
  }
  assert( buffers.last.size == 1, "Should reduce to 1 output" )
  io.dataOut.bits := buffers.last.head
  val vld = ShiftRegister( cntr > 0.U, cycles, false.B, true.B )
  val lastVld = RegInit( false.B ) // add one more cycle of valid
  lastVld := vld
  io.dataOut.valid := vld | lastVld
}
