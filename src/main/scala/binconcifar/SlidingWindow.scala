
package binconcifar

import chisel3._
import chisel3.util._

class SlidingWindow[ T <: Bits ]( genType : T, val grpSize : Int,
  val inSize : Int, val windowSize : Int, val stride : Int ) extends Module {

  private def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  private def lcm( a : Int, b : Int ) : Int = a * ( b / gcd( a, b ) )

  val noStrides = inSize.toDouble / stride
  val noOut = math.ceil( noStrides ).toInt
  val outSize = noOut * windowSize

  val minWinSize = lcm( stride, inSize )
  val effWindowSize = windowSize + ( noOut - 1 ) * stride
  val actualWindowSize = math.ceil( effWindowSize.toDouble / minWinSize ).toInt * minWinSize
  val noInBlocks = actualWindowSize / inSize
  val windowFilled = math.ceil( windowSize.toDouble / inSize ).toInt

  val io = IO( new Bundle {
    val dataIn = Input( Valid( Vec( inSize * grpSize, genType.cloneType ) ))
    val dataOut = Output( Valid( Vec( outSize * grpSize, genType.cloneType ) ))
    val vldMsk = Output( Vec( noOut, Bool() ) )
  })

  val windowRegs = List.fill( actualWindowSize ) { List.fill( grpSize ) { Reg( genType.cloneType ) } }
  // first inSize regs
  for ( i <- 0 until inSize ) {
    for ( j <- 0 until grpSize )
      windowRegs( i )( j ) := io.dataIn.bits( i*grpSize + j )
  }

  for ( i <- 0 until actualWindowSize - inSize ) {
    for ( j <- 0 until grpSize )
      windowRegs( i + inSize )( j ) := windowRegs( i )( j )
  }

  val vecOut = Wire( Vec( outSize * grpSize, genType.cloneType ) )
  // just attach some default values
  for ( i <- 0 until outSize ) {
    for ( j <- 0 until grpSize )
      vecOut( i*grpSize + j ) := 0.U
  }

  private def getStrideCntr() : ( UInt, Bool, UInt ) = {
    val effWindowFilled = math.ceil( effWindowSize.toDouble / inSize ).toInt
    println( "effWindowFilled = " + effWindowFilled )
    val cntr = Counter( io.dataIn.valid, effWindowFilled + 1 )
    val initDone = RegInit( false.B )
    initDone := initDone | ( cntr._1 >= ( windowFilled - 1 ).U )

    printf( "cntr = %d\n", cntr._1 )
    printf( "initDone = %d\n", initDone )

    val uniquePos = minWinSize / stride
    val cntrMax = minWinSize / inSize

    val pureOffsets = ( 0 until uniquePos ).map( x => {
      // total number of inputs recieved so far
      val noNums = math.ceil( (x * stride + windowSize.toDouble ) / inSize ).toInt * inSize
      // offset to nearest stride
      val winOff = noNums - ( x * stride + windowSize.toDouble )
      println( "winOff = " + winOff )
      // find the number the counter is for input x
      val cntrPosOff = x * stride - inSize + ( windowSize % inSize)
      val intY = math.ceil( cntrPosOff.toDouble / inSize ).toInt
      println( "intY = " + intY )
      val y = winOff.toInt % inSize
      println( "y = " + y )
      ( intY, y )
    }).groupBy( x => x._1 ).toList.map( _._2 ).map( y => y.minBy( _._2 ) ) // ensure distinct yOff

    val minStart = pureOffsets.minBy( _._1 )
    val strideOffsets = pureOffsets.map( x => { ( x._1 - minStart._1, x._2 ) } )

    println( "strideOffsets = " + strideOffsets )

    val vldMsks = List.fill( noOut ) { RegInit( false.B ) }
    for ( idx <- 0 until noOut ) {
      val triggerReg = RegInit( false.B )
      val triggerCond = ( cntr._1 >= math.ceil( ( windowSize + idx * stride.toDouble ) / inSize ).toInt.U )
      triggerReg := triggerReg | triggerCond
      // and range is valid
      vldMsks( idx ) := vldMsks( idx ) | triggerCond | triggerReg
      // delay by one cyc to match other outputs
      io.vldMsk( idx ) := vldMsks( idx )
    }

    if ( cntrMax <= 1 )
      return ( 0.U( 1.W ), initDone, (minStart._2 * grpSize ).U )

    println( "cntrMax = " + cntrMax )

    val strideCntr = Counter( io.dataIn.valid & initDone, cntrMax )

    val strideOffset = Wire( UInt( log2Up( inSize * grpSize ).W ) )
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
          if ( endPtr >= actualWindowSize || startPtr >= inSize ) {
            println( "vldMsks( " + idx + " ) when " + yOff._1 )
            vldMsks( idx ) := false.B
          }
        }
      }
    }

    printf( "vldMsk = " )
    for ( d <- vldMsks )
      printf( "%d, ", d )
    printf( "\n" )
    ( strideCntr._1, strideVld & initDone, strideOffset )
  }

  val strideCntr = getStrideCntr()
  printf( "strideCntr = %d\n", strideCntr._1 )
  printf( "strideVld = %d\n", strideCntr._2 )
  printf( "strideOff = %d\n", strideCntr._3 )
  val windowComb = Vec( windowRegs.reduce( _ ++ _ ) )

  printf( "windowComb = " )
  for ( d <- windowComb )
    printf( "%d, ", d )
  printf( "\n" )

  printf( "windowRegs = " )
  for ( vg <- windowRegs ) {
    for ( d <- vg )
      printf( "%d, ", d )
  }
  printf( "\n" )


  // assign the output
  val vecSize = windowSize * grpSize
  for ( idx <- 0 until noOut ) {
    val windStart = idx * stride * grpSize
    val highNum = ( vecSize - 1 + windStart ).U( log2Up( actualWindowSize*grpSize ).W ) + strideCntr._3
    val lowNum = windStart.U( log2Up( actualWindowSize*grpSize ).W ) + strideCntr._3
    printf( "highNum = %d\n", highNum )
    printf( "lowNum = %d\n", lowNum )
    assert( highNum >= lowNum, "High num should be greater than low num" )
    // assert( highNum < actualWindowSize.U & lowNum >= 0.U, "Idxs should be 0 <= x < vecSize" )
    DynamicVecAssign( vecOut, ( vecSize * ( idx + 1 ) - 1 ).U, ( vecSize * idx ).U,
      windowComb, highNum, lowNum )
  }

  io.dataOut.bits := RegEnable( vecOut, io.dataIn.valid )
  io.dataOut.valid := RegNext( io.dataIn.valid & strideCntr._2 )

}
