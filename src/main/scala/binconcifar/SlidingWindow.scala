
package binconcifar

import chisel3._
import chisel3.util._

/** Take in "groupsPerCycle" groups of numbers of "grpSize" each cycle
  * Display a windowSize output of numbers every "stride" inputs
  * Ignore the first "noIgnore" inputs where noIgnore < groupsPerCycle
  * Send the first valid with windowSize - "displayBefore" inputs
  */
class SlidingWindow[ T <: Bits ](
  genType : T,
  val grpSize : Int,
  val groupsPerCycle : Int,
  val windowSize : Int,
  val stride : Int,
  val noIgnore : Int = 0,
  val displayBefore : Int = 0
) extends Module {

  val cyclesPerStride = groupsPerCycle.toDouble / stride
  val noWindowOut = math.ceil( cyclesPerStride ).toInt
  val outSize = noWindowOut * windowSize
  val minWinSize = BufferLayer.lcm( stride, groupsPerCycle )

  private def roundUpBy( numToRound : Int, roundWith : Int ) : Int = {
    math.ceil( numToRound.toDouble / roundWith ).toInt * roundWith
  }

  val padSize = windowSize - displayBefore
  Predef.assert( padSize > 0, "displayBefore must be less than windowSize" )

  val effectiveWindowSize = windowSize + ( noWindowOut - 1 ) * stride
  val actualWindowSize = roundUpBy( effectiveWindowSize, minWinSize )

  val io = IO( new Bundle {
    val dataIn = Input( Valid( Vec( groupsPerCycle * grpSize, genType.cloneType ) ))
    val windShift = Input( UInt( log2Up( minWinSize * grpSize ).W ) )
    val dataOut = Output( Valid( Vec( outSize * grpSize, genType.cloneType ) ))
    val vldMsk = Output( Vec( noWindowOut, Bool() ) )
  })

  val windowRegs = List.fill( actualWindowSize + groupsPerCycle + stride ) {
    List.fill( grpSize ) {
      Reg( genType.cloneType )
    }
  }
  for ( i <- 0 until windowRegs.size ) {
    for ( j <- 0 until grpSize ) {
      when ( io.dataIn.valid ) {
        if ( i < groupsPerCycle )
          windowRegs( i )( j ) := io.dataIn.bits( i*grpSize + j )
        else
          windowRegs( i )( j ) := windowRegs( i - groupsPerCycle )( j )
      }
    }
  }

  /** Calculate the offset from which to send the data this cycle
    */
  private def calcStrideOffsets() : ( List[(Int, Int)], (Int, Int) ) = {
    val uniquePos = minWinSize / stride

    val pureOffsets = ( 0 until uniquePos ).map( x => {
      val groupsRecieved = x * stride + padSize + noIgnore
      val validGroups = roundUpBy( groupsRecieved, groupsPerCycle ) - noIgnore
      val windowStart = validGroups - ( x * stride + padSize )
      val counterOffset = x * stride - groupsPerCycle + ( padSize + noIgnore ) % groupsPerCycle
      val cycleOffset = math.ceil( counterOffset.toDouble / groupsPerCycle ).toInt
      val inputOffset = windowStart % groupsPerCycle
      ( cycleOffset, inputOffset )
    }).groupBy( x => x._1 ).toList.map( y => y._2.minBy( _._2 ) )

    val startingOffset = pureOffsets.minBy( _._1 )
    val strideOffsets = pureOffsets.map( x => {
      ( x._1 - startingOffset._1, x._2 )
    })
    ( strideOffsets, startingOffset )
  }

  /** Keep track of the outputs and set up the valid signals
    */
  private def getStrideCntr() : ( Bool, UInt ) = {
    val effWindowGroups = effectiveWindowSize.toDouble + noIgnore
    val effWindowCycles = math.ceil(  effWindowGroups / groupsPerCycle ).toInt
    val windowFilled = math.ceil( ( padSize.toDouble + noIgnore ) / groupsPerCycle ).toInt
    val initCounter = Counter( io.dataIn.valid, effWindowCycles + 1 )
    val initDone = RegInit( false.B )
    val initCounterWrapped = RegInit( false.B )
    when ( io.dataIn.valid & initCounter._1 >= ( windowFilled - 1 ).U ) {
      initDone := true.B
    }
    when ( io.dataIn.valid & initCounter._2 ) {
      initCounterWrapped := true.B
    }

    val vldMsks = List.fill( noWindowOut ) { RegInit( false.B ) }
    for ( idx <- 0 until noWindowOut ) {
      val windowEndGroups = padSize + noIgnore + idx * stride
      val windowEndCycles = math.ceil( windowEndGroups.toDouble / groupsPerCycle ).toInt
      vldMsks( idx ) := initCounter._1 >= windowEndCycles.U | initCounterWrapped
      io.vldMsk( idx ) := vldMsks( idx )
    }

    val strideCntrMax = minWinSize / groupsPerCycle
    val ( strideOffsets, startingOffset ) = calcStrideOffsets()

    if ( strideCntrMax <= 1 )
      return ( initDone & io.dataIn.valid, (startingOffset._2 * grpSize ).U )

    val strideCntr = Counter( io.dataIn.valid & initDone, strideCntrMax )

    val inputOffset = Wire( UInt( log2Up( groupsPerCycle * grpSize ).W ) )
    val outputValid = Wire( Bool() )
    inputOffset := 0.U
    outputValid := false.B
    for ( yOff <- strideOffsets ) {
      when ( strideCntr._1 === yOff._1.U ) {
        outputValid := initDone & io.dataIn.valid
        inputOffset := ( yOff._2 * grpSize ).U
        for ( idx <- 0 until noWindowOut ) {
          val startPtr = yOff._2 + idx*stride
          val endPtr = startPtr + windowSize - 1
          if ( endPtr >= actualWindowSize || startPtr >= groupsPerCycle )
            vldMsks( idx ) := false.B
        }
      }
    }

    ( outputValid, inputOffset )
  }

  val ( outputValid, inputOffset ) = getStrideCntr()
  val windowComb = Vec( windowRegs.reduce( _ ++ _ ) )

  val dataOutput = Wire( Vec( outSize * grpSize, genType.cloneType ) )
  val outDefault = Reg( genType.cloneType )
  for ( i <- 0 until outSize ) {
    for ( j <- 0 until grpSize )
      dataOutput( i*grpSize + j ) := outDefault
  }

  val noWindowInputs = windowSize * grpSize
  val idxWidth = log2Up( windowComb.size ).W

  for ( idx <- 0 until noWindowOut ) {
    val windStart = idx * stride * grpSize
    val highNum = ( noWindowInputs - 1 + windStart ).U( idxWidth ) + inputOffset + io.windShift
    val lowNum = windStart.U( idxWidth ) + inputOffset + io.windShift
    assert( highNum >= lowNum, "High num should be greater than low num" )
    assert( highNum < windowComb.size.U, "High num should be in window range" )
    DynamicVecAssign(
      dataOutput, ( noWindowInputs * ( idx + 1 ) - 1 ).U, ( noWindowInputs * idx ).U,
      windowComb, highNum, lowNum
    )
  }

  io.dataOut.bits := RegEnable( dataOutput, io.dataIn.valid )
  io.dataOut.valid := RegNext( outputValid )

}
