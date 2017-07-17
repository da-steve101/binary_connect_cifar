
package binconcifar

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object BufferLayer {
  def calcOutSize( outFormat : ( Int, Int, Int ),
    stride : Int, tPut : Double ) : Int = {
    outFormat._1 * outFormat._2 * outFormat._3 * {
      if ( tPut >= math.pow( stride, 2 ) )
        tPut / math.pow( stride, 2 )
      else
        1
    }.toInt
  }
}

class BufferLayer( val imgSize : Int, inSize : Int, val outFormat : (Int, Int, Int), qSize : Int,
  stride : Int, padding : Boolean, tPut : Double ) extends NNLayer( tPut, inSize,
    BufferLayer.calcOutSize( outFormat, stride, tPut ), true )   {

  Predef.assert( outFormat._1 == outFormat._2, "Must be square conv for now" )

  val debug = false

  val bufferedInput = {
    if ( throughput < 1 ) {
      val mySer = Serializer( dtype, inSize, (inSize/throughput).toInt )
      val dcpIO = Wire( Decoupled( mySer.io.dataOut.bits ) )
      dcpIO.bits := mySer.io.dataOut.bits
      dcpIO.valid := mySer.io.dataOut.valid
      dcpIO
    } else
      io.dataIn
  }

  /* Buffer up the input to give a square
   */
  val inQueue = Queue( bufferedInput, qSize )

  val ready = io.dataOut.ready
  inQueue.ready := ready
  val nextData = inQueue.valid & ready

  def zeroedSR( n : Int, b : Bool ) : Bool = {
    val zeroInitSR = List.fill( n ) { RegInit( false.B ) }
    zeroInitSR.head := b
    for ( idx <- 0 until zeroInitSR.size - 1 )
      zeroInitSR( idx + 1 ) := zeroInitSR( idx )
    zeroInitSR.last
  }

  def getValid( nxt : Bool ) : Bool = {
    val colCntr = Counter( nxt, imgSize )
    val rowCntr = Counter( colCntr._2, imgSize )
    val vldOut = Wire( Bool() )

    if ( debug ) {
      printf( "rowCntr = %d\n", rowCntr._1 )
      printf( "colCntr = %d\n", colCntr._1 )
    }

    val padSize = ( outFormat._2 - 1 )/ 2
    vldOut := nxt
    if ( padding ) {
      vldOut := true.B
    } else {
      when ( colCntr._1 < padSize.U ||
        colCntr._1 >= ( imgSize - padSize ).U ) {
        vldOut := false.B
      }
      when ( rowCntr._1 < padSize.U ||
        rowCntr._1 >= ( imgSize - padSize ).U ) {
        vldOut := false.B
      }
    }
    zeroedSR( padSize * imgSize + outFormat._2 - padSize, vldOut )
  }

  // valid should be delayed using counter ...
  io.dataOut.valid := getValid( nextData )

  val memBuffers = ArrayBuffer[Vec[T]]()
  memBuffers += inQueue.bits

  val bufferSize = {
    if ( throughput < 1 )
      imgSize
    else
      ( imgSize / throughput ).toInt
  }

  if ( debug )
    printf( "nextData = %d\n", nextData )

  // create a mem to buffer the data
  while ( memBuffers.size < outFormat._1 )
    memBuffers += MemShiftRegister( memBuffers.last, bufferSize, nextData )

  override def latency : Int = {
    // can define it ...
    new ChiselExecutionFailure( "Cannot get latency for BufferLayer as Queue means it is undefined" )
    -1
  }



  /** Responsible for buffering a vec that has multiple outputs
    */
  def bufferVec( vecIn : Vec[T], stride : Int ) : List[Vec[Vec[T]]] = {
    // consider stride and throughput ( is vec size )

    // cases to consider, stride = 1, tPut = 2, stride = 2, tPut = 3, stride = 2, tPut = 4
    val noOut = throughput.toInt

    // partition vecIn into its dims
    val vecGrp = vecIn.grouped( outFormat._3 ).toList.map( Vec( _ ) )
    val vecGrpReg = vecGrp.map( vg => { RegEnable( vg, nextData ) })
    val vecComb = vecGrpReg ++ vecGrp // combined vec of last 2 cycs

    if ( debug ) {
      printf( "vecComb = " )
      for ( x <- vecComb ) {
        for ( y <- x  )
          printf( "%d, ", y )
      }
      printf( "\n" )
    }

    // buffer the regs
    val bufferRegs = ( 0 until noOut ).map( idx => {
      val tmpRegs = List.fill( outFormat._2 ) { List.fill( outFormat._3 ) { Reg( dtype.cloneType ) } }
      when ( nextData ) {
        for ( i <- 0 until noOut ) {
          for ( j <- 0 until outFormat._3 )
            tmpRegs( i )( j ) := vecComb( vecGrp.size - idx + noOut - i - 1 )( j )
        }
        for ( i <- 0 until outFormat._2 - noOut ) {
          for ( j <- 0 until outFormat._3 )
            tmpRegs( i + noOut )( j ) := tmpRegs( i )( j )
        }
      }
      if ( debug ) {
        for ( a <- tmpRegs.zipWithIndex ) {
          printf( "tmpRegs( " + idx + " )( " + a._2 + " ) = " )
          for ( b <- a._1 )
            printf( "%d,", b )
          printf( "\n" )
        }
      }

      Vec( tmpRegs.map( Vec( _ ) ) )
    }).toList

    bufferRegs
  }

  val bufferedVecs = memBuffers.map( mb => bufferVec( mb, stride ) )
  val outVecs = ( 0 until bufferedVecs(0).size ).toList.map( i => bufferedVecs.map( x => x(i) ) )
  val joinedVecs = outVecs.map( x => x.reduce( ( a, b ) => Vec( a ++ b ) ).reverse.reduce( ( a, b ) => Vec( a ++ b ) ) )

  if ( debug ) {
    for ( mb <- memBuffers.zipWithIndex ) {
      printf( "memBuffers( " + mb._2 + " ) = " )
      for ( v <- mb._1 )
        printf( "%d, ", v )
      printf( "\n" )
    }
  }

  io.dataOut.bits := joinedVecs.reverse.reduce( ( a, b ) => Vec( a ++ b ) )

}
