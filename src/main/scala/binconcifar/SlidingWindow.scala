
package binconcifar

import chisel3._
import chisel3.util._

class SlidingWindow[ T <: Bits ]( genType : T, val grpSize : Int,
  val inSize : Int, val windowSize : Int, val stride : Int ) extends Module {

  /* Takes inputs in raw numbers
   * These numbers are grouped into pixels of grpSize
   */

  private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  private def lcm( a : Int, b : Int ) : Int = a * ( b / gcd( a, b ) )

  // calculate the number of outputs needed to keep up with the input
  val noStrides = inSize.toDouble / stride
  val noOut = math.ceil( noStrides ).toInt
  // calculate the number of pixel outputs
  val outSize = noOut * windowSize

  val minWinSize = lcm( stride, inSize )
  // calculate the effective window size by the number of pixels that have to be accessed each cycle
  val effWindowSize = windowSize + ( noOut - 1 ) * stride
  // calculate the actual window size by rounding up so that it is a multiple of inSize and stride
  val actualWindowSize = math.ceil( effWindowSize.toDouble / minWinSize ).toInt * minWinSize
  // calculate the number of cycles it takes to fill the window
  val noInBlocks = actualWindowSize / inSize
  // calculate how many cycles to fill the first window
  val windowFilled = math.ceil( windowSize.toDouble / inSize ).toInt

  val io = IO( new Bundle {
    val dataIn = Input( Valid( Vec( inSize * grpSize, genType.cloneType ) ))
    val dataOut = Output( Valid( Vec( outSize * grpSize, genType.cloneType ) ))
    val vldMsk = Output( Vec( noOut, Bool() ) )
  })

  // store data in registers
  val windowRegs = List.fill( actualWindowSize ) { List.fill( grpSize ) { Reg( genType.cloneType ) } }
  // for the first inSize regs, take directly from the input
  for ( i <- 0 until inSize ) {
    for ( j <- 0 until grpSize ) {
      when ( io.dataIn.valid ) {
        windowRegs( i )( j ) := io.dataIn.bits( i*grpSize + j )
      }
    }
  }

  // for the rest of the registers, shift the inputs along depending on the rate of input
  for ( i <- 0 until actualWindowSize - inSize ) {
    for ( j <- 0 until grpSize ) {
      when ( io.dataIn.valid ) {
        windowRegs( i + inSize )( j ) := windowRegs( i )( j )
      }
    }
  }

  // a vector to assign the output to
  val vecOut = Wire( Vec( outSize * grpSize, genType.cloneType ) )
  // just attach some default values
  for ( i <- 0 until outSize ) {
    for ( j <- 0 until grpSize )
      vecOut( i*grpSize + j ) := 0.U
  }

  /** Calculate the offset from which to send the data this cycle
    */
  private def calcStrideOffsets() : ( List[(Int, Int)], (Int, Int) ) = {
    // calculate the number of offsets that the window can have
    val uniquePos = minWinSize / stride

    val pureOffsets = ( 0 until uniquePos ).map( x => {
      // total number of inputs recieved so far
      val noNums = math.ceil( (x * stride + windowSize.toDouble ) / inSize ).toInt * inSize
      // offset to nearest stride
      val winOff = noNums - ( x * stride + windowSize.toDouble )
      // find the number the counter is for input x
      val cntrPosOff = x * stride - inSize + ( windowSize % inSize)
      // calculate the cycle offset
      val intY = math.ceil( cntrPosOff.toDouble / inSize ).toInt
      // calculate the bit offset
      val y = winOff.toInt % inSize
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
    val effWindowFilled = math.ceil( effWindowSize.toDouble / inSize ).toInt
    // a counter to count for initialization
    val cntr = Counter( io.dataIn.valid, effWindowFilled + 1 )
    // a register to check when there is enough data in the window to start outputting
    val initDone = RegInit( false.B )
    when ( io.dataIn.valid ) {
      initDone := initDone | ( cntr._1 >= ( windowFilled - 1 ).U )
    }

    val cntrMax = minWinSize / inSize

    val res = calcStrideOffsets()
    val strideOffsets = res._1
    val minStart = res._2

    // when there are multiple outputs, have a vector to indicate which outputs are valid
    val vldMsks = List.fill( noOut ) { RegInit( false.B ) }
    for ( idx <- 0 until noOut ) {
      // set up trigger to indicate when each output is initialized
      val cntrTrigger = math.ceil( ( windowSize + idx * stride.toDouble ) / inSize ).toInt - 1
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
  for ( idx <- 0 until noOut ) {
    val windStart = idx * stride * grpSize
    val highNum = ( vecSize - 1 + windStart ).U( log2Up( actualWindowSize*grpSize ).W ) + strideCntr._3
    val lowNum = windStart.U( log2Up( actualWindowSize*grpSize ).W ) + strideCntr._3
    assert( highNum >= lowNum, "High num should be greater than low num" )
    DynamicVecAssign( vecOut, ( vecSize * ( idx + 1 ) - 1 ).U, ( vecSize * idx ).U,
      windowComb, highNum, lowNum )
  }

  io.dataOut.bits := RegEnable( vecOut, io.dataIn.valid )
  io.dataOut.valid := RegNext( io.dataIn.valid & strideCntr._2 )

}
