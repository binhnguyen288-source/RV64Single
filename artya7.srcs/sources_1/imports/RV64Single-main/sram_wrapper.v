`default_nettype none

module singleportx8 #(
    parameter DEPTH = 0
)(
    input wire clk,
    input wire rstn,
    input wire wren,
    input wire ce,
    input wire[$clog2(DEPTH)-1:0] adr,
    input wire[7:0] din,
    output reg[7:0] dout
);
    reg[7:0] mem[DEPTH];
    for (genvar i = 0; i < DEPTH; ++i) begin
        always_ff @(posedge clk or negedge rstn)
            if (!rstn)
                mem[i] <= 8'd0;
            else if (ce && wren && i == adr)
                mem[i] <= din;
    end
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            dout <= '0;
        else if (ce & !wren)
            dout <= mem[adr];

    
endmodule

module SinglePortMemx32 #(
    parameter DEPTH = 2
)(
    output wire[31:0] dout,
    input wire clk,
    input wire rstn,
    input wire ce,
    input wire wren,
    input wire[3:0] wstrobe,
    input wire [$clog2(DEPTH)-1:0] addr,
    input wire [31:0] din
);
    for (genvar i = 0; i < 4; ++i) begin
        singleportx8 #(.DEPTH(DEPTH)) sp0(
            .clk, .wren(wren & wstrobe[i]), .ce, .adr(addr), .din(din[8*i+:8]), .dout(dout[8*i+:8]), .rstn
        );
    end
endmodule


module SinglePortMemx64 #(
    parameter DEPTH = 2
)(
    output wire[63:0] dout,
    input wire clk,
    input wire rstn,
    input wire ce,
    input wire wren,
    input wire[7:0] wstrobe,
    input wire [$clog2(DEPTH)-1:0] addr,
    input wire [63:0] din
);

    SinglePortMemx32 #(.DEPTH(DEPTH)) lo32(
        .dout(dout[0+:32]),
        .clk,
        .rstn,
        .ce,
        .wren,
        .wstrobe(wstrobe[0+:4]),
        .addr,
        .din(din[0+:32])
    );

    SinglePortMemx32 #(.DEPTH(DEPTH)) hi32(
        .dout(dout[32+:32]),
        .clk,
        .rstn,
        .ce,
        .wren,
        .wstrobe(wstrobe[4+:4]),
        .addr,
        .din(din[32+:32])
    );
    
endmodule
module singleportx82 #(
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
module RSPB18_512X32M4_G1 (
		input wire CLK,
		input wire ME,	//input			: Master Enable (1: selected; 0: not selected)
		input wire[10:0] ADR,
		input wire WE,	//input			: Write Enable (1: Write;  0: Read)
		input wire[3:0] WEM,	//input [3:0]	: Write Enable Mask (1: data in; 0: data not in)
		input wire[31:0] D,	//input [31:0]	: Write Data
		input wire OE,	//input			: Output Enable (1: enable; 0: disable)
		output wire[31:0] Q	//output [31:0]	: Read Data
	);

    for (genvar i = 0; i < 4; ++i) begin
        singleportx82 #(.DEPTH(2048)) sp0(
            .clk(CLK),
            .ce(ME),
            .wren(WE & WEM[i]),
            .adr(ADR),
            .din(D[8*i+:8]),
            .dout(Q[8*i+:8])
        );
    end
endmodule


module Scratch4096(
	input wire clk,
    input wire rstn,

    input wire[31:0] addr,
    input wire req,
    input wire wren,
    output wire gnt,
    input wire[63:0] wdata,
    input wire[7:0] wstrb,
    output wire[63:0] rdata,
    output reg rvalid,

    input wire[31:0] addr2,
    input wire addr_valid2,
    output wire addr_ready2,
    output reg rvalid2,
    output wire[31:0] rdata2
);

	
	always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid2 <= 1'b0;
        else
            rvalid2 <= addr_valid2 & addr_ready2;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid <= 1'b0;
        else
            rvalid <= req & gnt;

    reg addr2_sel_hi;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            addr2_sel_hi <= 1'b0;
        else
            addr2_sel_hi <= addr2[2];
    assign addr_ready2 = 1'b1;
    assign gnt = !addr_valid2;
    wire sram_enable = addr_valid2 && addr_ready2 || req && gnt;


    wire[31:0] sram_addr = addr_valid2 ? addr2 : addr;
    wire sram_wren = wren & req & gnt;

    wire[63:0] dout;
    RSPB18_512X32M4_G1 sram_lo (
		.CLK	(clk),
		.ME		(sram_enable),	//input			: Master Enable (1: selected; 0: not selected)
		.ADR	(sram_addr[3+:11]),	//input [8:0]	: Address
		.WE		(sram_wren),	//input			: Write Enable (1: Write;  0: Read)
		.WEM	(wstrb[0+:4]),	//input [3:0]	: Write Enable Mask (1: data in; 0: data not in)
		.D		(wdata[0+:32]),	//input [31:0]	: Write Data
		.OE		(1'b1),	//input			: Output Enable (1: enable; 0: disable)
		.Q		(dout[0+:32])	//output [31:0]	: Read Data
	);

    RSPB18_512X32M4_G1 sram_hi (
		.CLK	(clk),
		.ME		(sram_enable),	//input			: Master Enable (1: selected; 0: not selected)
		.ADR	(sram_addr[3+:11]),	//input [8:0]	: Address
		.WE		(sram_wren),	//input			: Write Enable (1: Write;  0: Read)
		.WEM	(wstrb[4+:4]),	//input [3:0]	: Write Enable Mask (1: data in; 0: data not in)
		.D		(wdata[32+:32]),	//input [31:0]	: Write Data
		.OE		(1'b1),	//input			: Output Enable (1: enable; 0: disable)
		.Q		(dout[32+:32])	//output [31:0]	: Read Data
	);
    assign rdata2 = addr2_sel_hi ? dout[32+:32] : dout[0+:32];
    assign rdata = dout;


endmodule
