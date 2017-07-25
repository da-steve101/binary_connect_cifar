
package binconcifar

import chisel3._
import chisel3.util._

class SlidingWindow[ T <: Bits ]( genType : T, val grpSize : Int,
  val inSize : Int, val windowSize : Int, val stride : Int ) extends Module {

  val noStrides = inSize.toDouble / stride
  val noOut = math.ceil( noStrides ).toInt
  val outSize = noOut * windowSize
  val io = IO( new Bundle {
    val dataIn = Input( Valid( Vec( inSize * grpSize, genType.cloneType ) ))
    val dataOut = Output( Valid( Vec( outSize * grpSize, genType.cloneType ) ))
    val vldMsk = Output( Valid( Vec( noOut, Bool() ) ))
  })

  val vecGrp = ( 0 until inSize ).map( i => {
    val tmpWire = Wire( Vec( grpSize, genType.cloneType ) )
    for ( j <- 0 until grpSize )
      tmpWire( j ) := io.dataIn.bits( i*grpSize + j )
    tmpWire
  }).toList
  val lastIn = vecGrp.map( vg => RegEnable( vg, io.dataIn.valid ) )
  val vecComb = lastIn ++ vecGrp // combined vec of last cyc
  val vecCombReg = vecComb.map( vg => RegEnable( vg, io.dataIn.valid ) )

  printf( "dataIn = " )
  for ( i <- 0 until inSize ) {
    printf( "( " )
    for ( j <- 0 until grpSize ) 
      printf( "%d, ", io.dataIn.bits( i*grpSize + j ) )
    printf( "), " )
  }
  printf( "\n" )

  printf( "vecCombReg = " )
  for ( i <- 0 until vecComb.size ) {
    printf( "( " )
    for ( d <- vecCombReg( i ) )
      printf( "%d, ", d )
    printf( "), " )
  }
  printf( "\n" )

  printf( "lastIn = " )
  for ( i <- 0 until lastIn.size ) {
    printf( "( " )
    for ( d <- lastIn( i ) )
      printf( "%d, ", d )
    printf( "), " )
  }
  printf( "\n" )

  val offset = windowSize % inSize

  val tmpRegInits = ( 0 until noOut ).map( idx => {
    ( 0 until inSize ).map( i => vecComb( vecGrp.size - idx*stride + i - offset ) ).toList
  }).toList.reverse

  for ( tri <- tmpRegInits.zipWithIndex ) {
    printf( "tmpRegInit( " + tri._2 + " ) = " )
    for ( x <- tri._1 ) {
      for ( y <- x )
        printf( "%d, ", y )
    }
    printf( "\n" )
  }

  // buffer the regs
  val bufferRegs = ( 0 until noOut ).map( idx => {
    val tmpRegs = List.fill( windowSize ) { List.fill( grpSize ) { Reg( genType.cloneType ) } }
    when ( io.dataIn.valid ) {
      for ( i <- 0 until windowSize - inSize ) {
        for ( j <- 0 until grpSize )
          tmpRegs( i )( j ) := tmpRegs( i + inSize )( j )
      }
      for ( i <- 0 until inSize ) {
        for ( j <- 0 until grpSize )
          tmpRegs( windowSize - inSize + i )( j ) := tmpRegInits( idx )( i )( j )
      }
    }
    for ( a <- tmpRegs.zipWithIndex ) {
      printf( "tmpRegs( " + idx + " )( " + a._2 + " ) = " )
      for ( b <- a._1 )
        printf( "%d,", b )
      printf( "\n" )
    }
    tmpRegs.reduce( _ ++ _ )
  }).toList.reduce( _ ++ _ )

  val cycToFill = math.ceil( windowSize.toDouble / inSize ).toInt
  val strideVld = {
    if ( noStrides < 1 ) {
      val cntr = Counter( io.dataIn.valid, stride )
      cntr._1 === ( cycToFill % stride ).U
    } else
      true.B
  }

  val initDone = RegInit( false.B )
  val initCntr = Counter( io.dataIn.valid, cycToFill )
  initDone := initDone | initCntr._2

  io.dataOut.bits := Vec( bufferRegs )
  io.dataOut.valid := strideVld & io.dataIn.valid & initDone

}
