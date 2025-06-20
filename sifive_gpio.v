module SIFIVE_GPIO(
    input wire clk,
    input wire rstn,

    input wire[11:0] addr,
    input wire req,
    input wire wren,
    output wire gnt,
    input wire[63:0] wdata,
    input wire[7:0] wstrb,
    output wire[63:0] rdata,
    output reg rvalid,
    inout wire[7:0] gpio_io
);
wire[7:0] gpio_io_w;
    for (genvar i = 0; i < 8; ++i) begin
        multi_buffering inst0(.in(gpio_io[i]), .out(gpio_io_w[i]));
    end
    reg[7:0] input_sync[2];
    always @(posedge clk or negedge rstn)
        if (!rstn)
            {input_sync[0], input_sync[1]} <= '0;
        else
            {input_sync[0], input_sync[1]} <= {input_sync[1], gpio_io_w};
    wire[7:0] input_val = input_sync[0];
    wire[7:0] input_en = '1;
    reg[7:0] output_en;
    reg[7:0] output_val;
    reg[7:0] out_xor;
    
    reg[7:0] output_en_q;
    reg[7:0] output_val_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            output_en_q <= '0;
            output_val_q <= '0;
        end else begin
            output_en_q <= output_en;
            output_val_q <= out_xor ^ output_val;
        end
    
    for (genvar i = 0; i < 8; ++i) begin
        assign gpio_io[i] = output_en_q[i] ? output_val_q[i] : 1'bZ;
    end
    reg[7:0] rdata_out;
    always @(posedge clk or negedge rstn)
	if (!rstn)
		rdata_out <= '0;
        else if (req & gnt)
            case ({addr[11:2], 2'b00})
                32'h0: rdata_out <= input_val;
                32'h4: rdata_out <= input_en;
                32'h8: rdata_out <= output_en;
                32'hc: rdata_out <= output_val;
                32'h40: rdata_out <= out_xor;
                default: rdata_out <= '0;
            endcase
    assign rdata = {2{32'(rdata_out)}};
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
//            input_en <= '0;
            output_en <= '0;
            output_val <= '0;
            out_xor <= '0;
        end else if (req & gnt & wren)
            case ({addr[11:2], 2'b00})
//                32'h0: input_en <= wdata[32+:32];
                32'h8: output_en <= wdata[0+:8];
                32'hc: output_val <= wdata[32+:8];
                32'h40:  out_xor <= wdata[0+:8];
                
            endcase
    always @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid <= 1'b0;
        else
            rvalid <= req & gnt;
    assign gnt = 1'b1;
endmodule
