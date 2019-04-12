`timescale 1ns / 1ps

module AddCarryBlock_4
#(
 parameter neg_io_b = 0,
 parameter neg_io_b_2 = 0
)
(
 input 	      clock,
 input 	      io_start,
 input [3:0]  io_a,
 input [3:0]  io_b,
 input [3:0]  io_a_2,
 input [3:0]  io_b_2,
 output [3:0] io_out,
 output [3:0] io_out_2
 );
   reg [1:0] carry_reg;
   wire [7:0] carry_out;
   wire [7:0] data_out;
   reg [7:0] data_out_reg;
   wire       c_in;
   wire       c_in_top;
   assign c_in = carry_reg[0];
   assign c_in_top = carry_reg[1];
   wire [3:0] io_b_neg;
   wire [3:0] io_b_2_neg;
   assign io_b_neg = neg_io_b ? ~io_b : io_b;
   assign io_b_2_neg = neg_io_b_2 ? ~io_b_2 : io_b_2;
CARRY8
#(
 .CARRY_TYPE( "DUAL_CY4" )
) carry8 (
   .CO(carry_out),
   .O(data_out),
   .CI(c_in), // 1-bit input: Lower Carry-In
   .CI_TOP(c_in_top), // 1-bit input: Upper Carry-In
   .DI({io_a, io_a_2}), // 8-bit input: Carry-MUX data in
   .S({io_b_neg, io_b_2_neg})
);
   assign io_out = data_out_reg[7:4];
   assign io_out_2 = data_out_reg[3:0];
always @( posedge clock ) begin
   if ( io_start ) begin
      carry_reg[1] <= neg_io_b;
      carry_reg[0] <= neg_io_b_2;
   end else begin
      carry_reg[1] <= carry_out[7];
      carry_reg[0] <= carry_out[3];
   end
   data_out_reg <= data_out;
end
endmodule
