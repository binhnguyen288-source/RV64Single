module RISCV_CLINT(
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
    output wire[63:0] out_mtime,
    output wire out_MTIP0,
    output wire out_MSIP0
);
    reg[63:0] mtime;
    reg[63:0] mtimecmp0;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            mtime <= 0;
        else
            mtime <= mtime + 1;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid <= 1'b0;
        else
            rvalid <= req & gnt;
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rdata <= '0;
        else if (req & gnt)
            case ({addr[31:3], 3'd0})
                32'h4000: rdata <= mtimecmp0;
                32'hBFF8: rdata <= mtime;
                default: rdata <= '0;
            endcase
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            mtimecmp0 <= 0;
        else if (req & gnt & wren)
            case ({addr[31:3], 3'd0})
                32'h4000: begin
                    if (wstrb[0])
                        mtimecmp0[0+:32] <= wdata[0+:32];
                    if (wstrb[4])
                        mtimecmp0[32+:32] <= wdata[32+:32];
                end
            endcase
    reg assert_MTIP0;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            assert_MTIP0 <= 1'b0;
        else
            assert_MTIP0 <= mtime >= mtimecmp0;
    
    assign gnt = 1'b1;
    assign out_mtime = mtime;
    assign out_MTIP0 = assert_MTIP0;
    assign out_MSIP0 = 1'b0;

endmodule