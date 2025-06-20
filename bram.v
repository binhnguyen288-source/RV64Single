module singleportx8 #(
    parameter DEPTH = 0
)(
    input wire clk,
    input wire wren,
    input wire ce,
    input wire[$clog2(DEPTH)-1:0] adr,
    input wire[7:0] din,
    output reg[7:0] dout
);
    reg[7:0] mem[DEPTH];
    always_ff @(posedge clk)
        if (ce)
            if (wren)
                mem[adr] <= din;
            else
                dout <= mem[adr];

    
endmodule

module RSPB18_128X32M4_G1 (
		input wire CLK,
		input wire ME,	//input			: Master Enable (1: selected; 0: not selected)
		input wire[6:0] ADR,
		input wire WE,	//input			: Write Enable (1: Write;  0: Read)
		input wire[3:0] WEM,	//input [3:0]	: Write Enable Mask (1: data in; 0: data not in)
		input wire[31:0] D,	//input [31:0]	: Write Data
		input wire OE,	//input			: Output Enable (1: enable; 0: disable)
		output wire[31:0] Q	//output [31:0]	: Read Data
	);

    for (genvar i = 0; i < 4; ++i) begin
        singleportx8 #(.DEPTH(128)) sp0(
            .clk(CLK),
            .ce(ME),
            .wren(WE & WEM[i]),
            .adr(ADR),
            .din(D[8*i+:8]),
            .dout(Q[8*i+:8])
        );
    end
endmodule