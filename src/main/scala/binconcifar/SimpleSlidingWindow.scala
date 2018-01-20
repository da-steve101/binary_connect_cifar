package binconcifar

import chisel3._
import chisel3.util._


/*
 Make a sliding window that only takes in a stride of 1 or 2 and
 A window size of 2 without padding and 3 with padding. TPut is arbitrary
 */

class SimpleSlidingWindow[ T <: Bits ](
  genType : T,
  val grpSize : Int,
  val groupsPerCycle : Int,
  val windowSize : Int,
  val stride : Int
) extends Module {

  val cyclesPerStride = groupsPerCycle.toDouble / stride
  val noWindowOut = math.ceil( cyclesPerStride ).toInt
  val outSize = noWindowOut * windowSize

  Predef.assert( ( groupsPerCycle % stride == 0 ) ||
    ( stride % groupsPerCycle == 0 ), "Tput must be divisible by stride" )
  Predef.assert( (windowSize == 3 && stride == 1) ||
    ( windowSize == 2 && stride == 2 ), "WindowSize and stride are restricted" )

  val actualWindowSize = {
    if ( windowSize == 3 )
      2 * groupsPerCycle + 1
    else
      scala.math.max( groupsPerCycle, windowSize )
  }

  val io = IO( new Bundle {
    val dataIn = Flipped( Valid( Vec( groupsPerCycle * grpSize, genType.cloneType ) ))
    val dataOut = Decoupled( Vec( outSize * grpSize, genType.cloneType ) )
  })

  val nextData = io.dataIn.valid & io.dataOut.ready
  val windowRegs = List.fill( actualWindowSize ) {
    List.fill( grpSize ) {
      Reg( genType.cloneType )
    }
  }
  when ( nextData ) {
    for ( i <- 0 until windowRegs.size ) {
      for ( j <- 0 until grpSize ) {
        if ( i < groupsPerCycle )
          windowRegs( i )( j ) := io.dataIn.bits( i*grpSize + j )
        else
          windowRegs( i )( j ) := windowRegs( i - groupsPerCycle )( j )
      }
    }
  }

  // wait 1 cycle before filled for windowSize == 3
  // if tPut == 1 and stride == 2 need to mask valid
  val firstIsValid = !( groupsPerCycle < stride || windowSize == 3 )
  val vldReg = RegInit( firstIsValid.B )
  def getMask() : Bool = {
    if ( groupsPerCycle < stride ) { // only needed when grps/cyc = 1 stride = 2
      val mask = RegInit( false.B )
      when ( nextData ) {
        mask := !mask
      }
      mask
    } else if ( windowSize == 3 ) {
      val initReg = RegInit( false.B )
      when ( nextData ) {
        initReg := true.B
      }
      initReg
    } else {
      true.B
    }
  }

  when( io.dataOut.ready ) {
    vldReg := io.dataIn.valid & getMask()
  }

  // get output eg) windowSize = 3 and tPut = 4
  // 3 2 1 0 _
  // 7  6  5 4 3 2 1 0 _ // output 0-3
  // 11 10 9 8 7 6 5 4 3 // output 7-4
  val dataOut = {
    if ( windowSize == 3 ) {
      // group into blocks of 3
      val windOut = ( 0 until groupsPerCycle ).toList.map( idx => {
        ( 0 until windowSize ).toList.map( ws => {
          windowRegs( idx + groupsPerCycle + ws - 1 ).toList
        }).reduce( _ ++ _ )
      }).reduce( _ ++ _ )
      windOut
    } else {
      windowRegs.reduce( _ ++ _ )
    }
  }

  io.dataOut.bits := Vec( dataOut )
  io.dataOut.valid := vldReg
}
