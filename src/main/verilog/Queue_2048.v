
module Queue_2048
(
 input 		 clock,
 input 		 reset,
 input [2047:0]  io_enq_bits,
 input 		 io_enq_valid,
 output 	 io_enq_ready,
 output [2047:0] io_deq_bits,
 output 	 io_deq_valid,
 input 		 io_deq_ready
 );
   wire 	 q1_rdy;
   wire 	 q2_rdy;
   wire 	 q1_vld;
   wire 	 q2_vld;
   wire [1023:0] bitsIn_0_1023;
   wire [1023:0] bitsIn_1024_2047;
   wire [1023:0] bitsOut_0_1023;
   wire [1023:0] bitsOut_1024_2047;

   assign bitsIn_0_1023 = io_enq_bits[1023:0];
   assign bitsIn_1024_2047 = io_enq_bits[2047:1024];
   assign io_deq_valid = q1_vld & q2_vld;
   assign io_enq_ready = q1_rdy & q2_rdy;
   assign io_deq_bits = { bitsOut_1024_2047, bitsOut_0_1023 };

   Queue_1024 queue1
     (
      .clock( clock ),
      .reset( reset ),
      .io_enq_bits( bitsIn_0_1023 ),
      .io_enq_valid( io_enq_valid ),
      .io_enq_ready( q1_rdy ),
      .io_deq_bits( bitsOut_0_1023 ),
      .io_deq_valid( q1_vld ),
      .io_deq_ready( io_deq_ready )
      );

   Queue_1024 queue2
     (
      .clock( clock ),
      .reset( reset ),
      .io_enq_bits( bitsIn_1024_2047 ),
      .io_enq_valid( io_enq_valid ),
      .io_enq_ready( q2_rdy ),
      .io_deq_bits( bitsOut_1024_2047 ),
      .io_deq_valid( q2_vld ),
      .io_deq_ready( io_deq_ready )
      );

endmodule
