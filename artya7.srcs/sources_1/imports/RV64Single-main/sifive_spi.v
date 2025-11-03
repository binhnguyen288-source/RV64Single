
`default_nettype none
module spi_piso #(
    parameter INIT = 0,
    parameter WIDTH = 8
)(
    input wire clk,
    input wire rstn,
    input wire[WIDTH-1:0] in,
    output wire shifted_last,
    input wire padding,
    input wire load,
    input wire shift,
    output wire out
);
    reg[WIDTH-1:0] shift_out;
    reg[WIDTH-1:0] shifted_last_q;
    
    reg[WIDTH-1:0] shift_out_nxt;
    reg[WIDTH-1:0] shift_last_nxt;
    always_comb begin
        shift_out_nxt = shift_out;
        shift_last_nxt = shifted_last_q;
        if (load) begin
            shift_out_nxt = in;
            shift_last_nxt = WIDTH'(1);
        end else if (shift) begin
            shift_out_nxt = {shift_out[WIDTH-2:0], padding};
            shift_last_nxt = {shifted_last_q[WIDTH-2:0], 1'b1};
        end
    end
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            shift_out <= INIT;
            shifted_last_q <= 1 << (WIDTH-1);
        end else begin
            shift_out <= shift_out_nxt;
            shifted_last_q <= shift_last_nxt;
        end
        
    reg out_reg;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            out_reg <= ~INIT[WIDTH-1];
        else
            out_reg <= shift_out_nxt[WIDTH-1];
    
    assign shifted_last = shifted_last_q[WIDTH-1];
    assign out = out_reg;

endmodule


module spi_sipo (
    input wire clk,
    input wire rstn,
    input wire in,
    input wire shift,
    
    output reg[7:0] out,
    output reg out_valid
);
    reg in_q;
    reg in_qq;
    reg shift_q;
    reg shift_qq;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            in_q <= 1'b1;
            shift_q <= 1'b0;
        end 
        else begin
            shift_q <= shift;
            in_q <= in;
        end
      always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            in_qq <= 1'b1;
            shift_qq <= 1'b0;
        end 
        else begin
            shift_qq <= shift_q;
            in_qq <= in_q;
        end
    reg[2:0] ctr;
    always @(posedge clk or negedge rstn)
        if (!rstn)
            ctr <= 0;
        else if (shift_qq)
            ctr <= ctr + 1'b1;
    
    always @(posedge clk or negedge rstn)
        if (!rstn)
            out <= '0;
        else if (shift_qq)
            out <= {out[6:0], in_qq};
    always @(posedge clk or negedge rstn)
        if (!rstn)
            out_valid <= 1'b0;
        else
            out_valid <= shift_qq && ctr == 7;

endmodule
module SIFIVE_SPI(
    input wire clk,
    input wire rstn,

    input wire[31:0] addr,
    input wire req,
    input wire wren,
    output wire gnt,
    input wire[63:0] wdata,
    input wire[7:0] wstrb,
    output reg[63:0] rdata,
    output reg rvalid,
    output wire spi_cs,
    output reg spi_clk,
    output wire spi_mosi,
    input wire spi_miso
    
);
    wire tx_fifo_rden;
    reg[15:0] clk_ctr;
    reg[15:0] ctr_limit;
    reg[15:0] ctr_rising;
    wire clk_falling_nxt = clk_ctr >= ctr_limit;
    reg[11:0] sckdiv; // 0x0 fclk / 2(sckdiv+1)
    reg[1:0] sckmode; // 0x4[pha, pol]
    reg[1:0] csmode; // 0x18 0: auto, 2: hold
    wire tx_wready;
    wire[31:0] txfifo_read = {~tx_wready, 31'h0};
    wire[31:0] rxfifo_read;
    wire tx_rvalid;
    wire[7:0] tx_rdata;
    sync_fifo #(
        .WIDTH(8)
    ) tx_fifo_inst(
		.wdata(wdata[7:0]), //input [7:0] Data
		.clk(clk), //input Clk
		.wvalid(addr == 32'h48 && wren && req && gnt), //input WrEn
		.rready(tx_fifo_rden), //input RdEn
		.rstn(rstn), //input Reset
		.rdata(tx_rdata), //output [7:0] Q
		.rvalid(tx_rvalid), //output Empty
		.wready(tx_wready) //output Full
	);
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
            sckdiv <= '0;
            sckmode <= '0;
            csmode <= '0;
        end else if (req & wren & gnt)
            case (addr)
                32'h0: sckdiv <= wdata[0+:32];
                32'h4: sckmode <= wdata[32+:32];
                32'h18: csmode <= wdata[0+:32];
            endcase
    always @(posedge clk or negedge rstn)
	if (!rstn)
		rdata <= '0;
        else if (req & gnt)
            case (addr)
                32'h0: rdata <= sckdiv;
                32'h4: rdata <= {sckmode, 32'h0};
                32'h18: rdata <= csmode;
                32'h48: rdata <= txfifo_read;
                32'h4c: rdata <= {rxfifo_read, 32'h0};
                default: rdata <= '0;
            endcase
            
        
    

    wire[15:0] clk_ctr_nxt = (clk_ctr >= ctr_limit) ? '0 : clk_ctr + 1'b1;
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
            clk_ctr <= '0;
            ctr_limit <= 1;
            ctr_rising <= 1;
        end else begin
            clk_ctr <= clk_ctr_nxt;
            ctr_limit <= 2*sckdiv+1;
            ctr_rising <= sckdiv+1;
        end
    
    wire mosi_shifted_last;
    wire clk_rising_nxt = clk_ctr_nxt == ctr_rising;
    reg[3:0] clk_gate_cnt;
    assign tx_fifo_rden = tx_rvalid && clk_falling_nxt && mosi_shifted_last;
    always @(posedge clk or negedge rstn)
        if (!rstn)
            clk_gate_cnt <= '0;
        else
            clk_gate_cnt <= clk_gate_cnt + (tx_fifo_rden ? 8 : 0) - (clk_gate_cnt && clk_falling_nxt);
    
    
    always @(posedge clk or negedge rstn)
        if (!rstn)
            spi_clk <= 1'b0;
        else
            spi_clk <= clk_ctr_nxt >= ctr_rising && clk_gate_cnt;
    spi_piso #(
        .INIT(8'h0)
    ) mosi_piso(
        .clk,
        .rstn,
        .in(tx_rdata),
        .load(tx_fifo_rden),
        .shift(clk_falling_nxt),
        .shifted_last(mosi_shifted_last),
        .padding(1'b0),
        .out(spi_mosi)
    );
    spi_piso #(
        .INIT(9'h1FF),
        .WIDTH(9)
    ) cs_piso(
        .clk,
        .rstn,
        .in(9'd0),
        .load(tx_fifo_rden),
        .shift(clk_falling_nxt),
        .padding(csmode == 0),
        .shifted_last(),
        .out(spi_cs)
    );
    wire[7:0] miso_out;
    wire miso_out_valid;
    spi_sipo miso_sipo(
        .clk,
        .rstn,
        .in(spi_miso),
        .shift(clk_gate_cnt && clk_rising_nxt),
        .out(miso_out),
        .out_valid(miso_out_valid)
    );
    wire rx_fifo_rvalid;
    wire[7:0] rx_fifo_rdata;
    sync_fifo #(
        .WIDTH(8)
    ) rx_fifo_inst(
		.wdata(miso_out), //input [7:0] Data
		.clk(clk), //input Clk
		.wvalid(miso_out_valid), //input WrEn
		.rready(addr == 32'h4c && !wren && req && gnt), //input RdEn
		.rstn(rstn), //input Reset
		.rdata(rx_fifo_rdata), //output [7:0] Q
		.rvalid(rx_fifo_rvalid), //output Empty
		.wready() //output Full
	);
    assign rxfifo_read = {~rx_fifo_rvalid, 23'd0, rx_fifo_rdata};
    always @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid <= 1'b0;
        else
            rvalid <= req & gnt;
    assign gnt = 1'b1;
endmodule
