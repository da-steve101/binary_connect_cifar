
module Queue_1024
(
 input 		 clock,
 input 		 reset,
 input [1023:0]  io_enq_bits,
 input 		 io_enq_valid,
 output 	 io_enq_ready,
 output [1023:0] io_deq_bits,
 output 	 io_deq_valid,
 input 		 io_deq_ready
 );
   wire 	 not_rdy;
   assign io_enq_ready = !not_rdy;

   fifo_1024bits fifo_impl
     (
      .clk( clock ),
      .srst( reset ),
      .din( io_enq_bits ),
      .wr_en( io_enq_valid ),
      .rd_en( io_deq_ready & io_deq_valid ),
      .dout( io_deq_bits ),
      .full( not_rdy ),
      .valid( io_deq_valid )
      );
endmodule
