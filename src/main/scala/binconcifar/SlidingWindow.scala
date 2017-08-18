
package binconcifar

import chisel3._
import chisel3.util._

/** Take in "inSize" groups of numbers of "grpSize" each cycle
  * Display a windowSize output of numbers every "stride" inputs
  * Ignore the first "noIgnore" inputs where noIgnore < inSize
  * Send the first valid with windowSize - "displayBefore" inputs
  */
class SlidingWindow[ T <: Bits ]( genType : T, val grpSize : Int, val inSize : Int,
  val windowSize : Int, val stride : Int, val noIgnore : Int = 0, val displayBefore : Int = 0 ) extends Module {

  /* Takes inputs in raw numbers
   * These numbers are grouped into pixels of grpSize
   */

  // calculate the number of outputs needed to keep up with the input
  val noStrides = inSize.toDouble / stride
  val noOut = math.ceil( noStrides ).toInt
  // calculate the number of pixel outputs
  val outSize = noOut * windowSize
  val minWinSize = BufferLayer.lcm( stride, inSize )
  // the max stride counter
  val cntrMax = minWinSize / inSize

  val io = IO( new Bundle {
    val dataIn = Input( Valid( Vec( inSize * grpSize, genType.cloneType ) ))
    val windShift = Input( UInt( log2Up( minWinSize ).W ) )
    val dataOut = Output( Valid( Vec( outSize * grpSize, genType.cloneType ) ))
    val vldMsk = Output( Vec( noOut, Bool() ) )
  })

  val padSize = windowSize - displayBefore
  Predef.assert( padSize > 0, "displayBefore must be less than windowSize" )

  // calculate the effective window size by the number of pixels that have to be accessed each cycle
  val effWindowSize = windowSize + ( noOut - 1 ) * stride
  // calculate the actual window size by rounding up so that it is a multiple of inSize and stride
  val actualWindowSize = math.ceil( effWindowSize.toDouble / minWinSize ).toInt * minWinSize

  // store data in registers
  val windowRegs = List.fill( actualWindowSize + inSize + stride ) { List.fill( grpSize ) { Reg( genType.cloneType ) } }
  // for the first inSize regs, take directly from the input
  for ( i <- 0 until inSize ) {
    for ( j <- 0 until grpSize ) {
      when ( io.dataIn.valid ) {
        windowRegs( i )( j ) := io.dataIn.bits( i*grpSize + j )
      }
    }
  }

  // for the rest of the registers, shift the inputs along depending on the rate of input
  for ( i <- 0 until windowRegs.size - inSize ) {
    for ( j <- 0 until grpSize ) {
      when ( io.dataIn.valid ) {
        windowRegs( i + inSize )( j ) := windowRegs( i )( j )
      }
    }
  }

  // a vector to assign the output to
  val vecOut = Wire( Vec( outSize * grpSize, genType.cloneType ) )
  val outDefault = Reg( genType.cloneType )
  // just attach some default values
  for ( i <- 0 until outSize ) {
    for ( j <- 0 until grpSize )
      vecOut( i*grpSize + j ) := outDefault
  }

  /** Calculate the offset from which to send the data this cycle
    */
  private def calcStrideOffsets() : ( List[(Int, Int)], (Int, Int) ) = {
    // calculate the number of offsets that the window can have
    val uniquePos = minWinSize / stride

    val pureOffsets = ( 0 until uniquePos ).map( x => {
      // total number of inputs recieved so far
      val noNums = math.ceil( (x * stride + padSize.toDouble + noIgnore ) / inSize ).toInt * inSize - noIgnore
      // offset to nearest stride
      val winOff = noNums - ( x * stride + padSize )
      // find the number the counter is for input x after init is done
      val cntrPosOff = x * stride - inSize + ( padSize + noIgnore ) % inSize
      // calculate the cycle offset
      val intY = math.ceil( cntrPosOff.toDouble / inSize ).toInt
      // calculate the bit offset
      val y = winOff % inSize
      ( intY, y )
    }).groupBy( x => x._1 ).toList.map( _._2 ).map( y => y.minBy( _._2 ) ) // ensure only one each cycle

    // calculate the starting offset
    val minStart = pureOffsets.minBy( _._1 )
    // as the counter starts after initdone, the number of cycles needs to be shifted to start at 0
    ( pureOffsets.map( x => { ( x._1 - minStart._1, x._2 ) } ), minStart )
  }

  /** Keep track of the outputs and set up the valid signals
    */
  private def getStrideCntr() : ( UInt, Bool, UInt ) = {
    // calculate the number of cycles to fill the effective windowSize
    val effWindowFilled = math.ceil( ( effWindowSize.toDouble + noIgnore ) / inSize ).toInt
    // calculate how many cycles to fill the first window
    val windowFilled = math.ceil( ( padSize.toDouble + noIgnore ) / inSize ).toInt
    // a counter to count for initialization
    val cntr = Counter( io.dataIn.valid, effWindowFilled + 1 )
    // a register to check when there is enough data in the window to start outputting
    val initDone = RegInit( false.B )
    when ( io.dataIn.valid ) {
      initDone := initDone | ( cntr._1 >= ( windowFilled - 1 ).U )
    }

    val res = calcStrideOffsets()
    println( "strideOffset = " + res )
    val strideOffsets = res._1
    val minStart = res._2

    // when there are multiple outputs, have a vector to indicate which outputs are valid
    val vldMsks = List.fill( noOut ) { RegInit( false.B ) }
    for ( idx <- 0 until noOut ) {
      // set up trigger to indicate when each output is initialized
      val cntrTrigger = math.ceil( ( padSize + noIgnore + idx * stride.toDouble ) / inSize ).toInt - 1
      val triggerReg = RegInit( (cntrTrigger <= 0).B )
      val triggerCond = ( cntr._1 >= cntrTrigger.U )
      when( triggerCond & io.dataIn.valid ) {
        triggerReg := true.B
      }
      // if triggered then assume is valid
      vldMsks( idx ) := triggerReg
      // attach the outputs
      io.vldMsk( idx ) := vldMsks( idx )
    }

    // if this is a simple case ( inSize == stride ) then we are done
    if ( cntrMax <= 1 )
      return ( 0.U( 1.W ), initDone, (minStart._2 * grpSize ).U )

    // otherwise make a counter for the possible scenarios
    val strideCntr = Counter( io.dataIn.valid & initDone, cntrMax )

    // an index to indicate where to take the reg from
    val strideOffset = Wire( UInt( log2Up( inSize * grpSize ).W ) )
    // indicate if the data out is valid
    val strideVld = Wire( Bool() )
    strideOffset := 0.U
    strideVld := false.B
    for ( yOff <- strideOffsets ) {
      when ( strideCntr._1 === yOff._1.U ) {
        strideVld := true.B
        strideOffset := ( yOff._2 * grpSize ).U
        // when offset outside of range then set vldMsk false
        for ( idx <- 0 until noOut ) {
          val startPtr = yOff._2 + idx*stride
          val endPtr = startPtr + windowSize - 1
          if ( endPtr >= actualWindowSize || startPtr >= inSize )
            vldMsks( idx ) := false.B
        }
      }
    }

    ( strideCntr._1, strideVld & initDone, strideOffset )
  }

  val strideCntr = getStrideCntr()
  val windowComb = Vec( windowRegs.reduce( _ ++ _ ) )

  // assign the output
  val vecSize = windowSize * grpSize
  val idxWidth = log2Up( windowRegs.size*grpSize ).W

  for ( idx <- 0 until noOut ) {
    val windStart = idx * stride * grpSize
    val highNum = ( vecSize - 1 + windStart ).U( idxWidth ) + strideCntr._3 + io.windShift
    val lowNum = windStart.U( idxWidth ) + strideCntr._3 + io.windShift
    assert( highNum >= lowNum, "High num should be greater than low num" )
    assert( highNum < windowRegs.size.U, "High num should be in window range" )
    DynamicVecAssign( vecOut, ( vecSize * ( idx + 1 ) - 1 ).U, ( vecSize * idx ).U,
      windowComb, highNum, lowNum )
  }

  io.dataOut.bits := RegEnable( vecOut, io.dataIn.valid )
  io.dataOut.valid := RegNext( io.dataIn.valid & strideCntr._2 )

}
