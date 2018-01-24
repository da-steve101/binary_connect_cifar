
module Queue_4096
(
 input 		 clock,
 input 		 reset,
 input [4095:0]  io_enq_bits,
 input 		 io_enq_valid,
 output 	 io_enq_ready,
 output [4095:0] io_deq_bits,
 output 	 io_deq_valid,
 input 		 io_deq_ready
 );
   wire 	 q1_rdy;
   wire 	 q2_rdy;
   wire 	 q1_vld;
   wire 	 q2_vld;
   wire [2047:0] bitsIn_0_2047;
   wire [2047:0] bitsIn_2048_4095;
   wire [2047:0] bitsOut_0_2047;
   wire [2047:0] bitsOut_2048_4095;

   assign bitsIn_0_2047 = io_enq_bits[2047:0];
   assign bitsIn_2048_4095 = io_enq_bits[4095:2048];
   assign io_deq_valid = q1_vld & q2_vld;
   assign io_enq_ready = q1_rdy & q2_rdy;
   assign io_deq_bits = { bitsOut_2048_4095, bitsOut_0_2047 };
   
   Queue_2048 queue1
     (
      .clock( clock ),
      .reset( reset ),
      .io_enq_bits( bitsIn_0_2047 ),
      .io_enq_valid( io_enq_valid ),
      .io_enq_ready( q1_rdy ),
      .io_deq_bits( bitsOut_0_2047 ),
      .io_deq_valid( q1_vld ),
      .io_deq_ready( io_deq_ready )
      );

   Queue_2048 queue2
     (
      .clock( clock ),
      .reset( reset ),
      .io_enq_bits( bitsIn_2048_4095 ),
      .io_enq_valid( io_enq_valid ),
      .io_enq_ready( q2_rdy ),
      .io_deq_bits( bitsOut_2048_4095 ),
      .io_deq_valid( q2_vld ),
      .io_deq_ready( io_deq_ready )
      );

endmodule
