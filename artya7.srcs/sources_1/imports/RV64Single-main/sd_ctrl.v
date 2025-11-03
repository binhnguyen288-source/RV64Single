
module buffer_io_cell(
		input wire in,
		output wire out
);
    assign out = in;
//	ROHM18BUFP005 inst0(.A(in), .Y(out));
endmodule

module multi_buffering(
	input wire in,
	output wire out
);
	parameter FACTOR = 2;
	wire[FACTOR:0] in_w;
	assign in_w[FACTOR] = in;
	for (genvar i = 0; i < FACTOR; ++i) begin
		buffer_io_cell inst0(.in(in_w[i+1]), .out(in_w[i]));
	end
	assign out = in_w[0];
endmodule


module sram_piso_tristate #(
    parameter WIDTH = 0
)(
    input wire clk,
    input wire rstn,
    input wire shift,
    input wire[WIDTH-1:0] data_in,
    input wire[WIDTH-1:0] oen_in,
    input wire in_valid,
    inout wire pin_io,
    input wire capture,
    
    output reg[WIDTH-1:0] data_out
);

    
    reg[WIDTH-1:0] data_q;
    reg[WIDTH-1:0] oen_q;

    reg[WIDTH-1:0] data_nxt;
    reg[WIDTH-1:0] oen_nxt;
    always_comb
        if (in_valid) begin
            oen_nxt = oen_in;
            data_nxt = data_in;
        end else if (shift) begin
            oen_nxt = {oen_q[WIDTH-2:0], 1'b0};
            data_nxt = {data_q[WIDTH-2:0], 1'b0};
        end else begin
            oen_nxt = oen_q;
            data_nxt = data_q;
        end
    reg oen_buf; 
    reg data_buf;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            oen_q <= '0;
            data_q <= '0;
            oen_buf <= 1'b0;
            data_buf <= 1'b0;
        end else begin
            oen_buf <= oen_nxt[WIDTH-1];
            data_buf <= data_nxt[WIDTH-1];
            oen_q <= oen_nxt;
            data_q <= data_nxt;
        end
	wire pin_io_w;
	multi_buffering #(.FACTOR(4)) inst0(.in(pin_io), .out(pin_io_w));
    reg pin_io_in_q;
    reg pin_io_in_syn;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            pin_io_in_q <= 1'b0;
        else
            pin_io_in_q <= pin_io_w;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            pin_io_in_syn <= 1'b0;
        else
            pin_io_in_syn <= pin_io_in_q;
            
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            data_out <= '0;
        else if (capture)
            data_out <= {data_out[WIDTH-2:0], pin_io_in_syn};
    
    assign pin_io = oen_buf ? data_buf : 1'bZ;
endmodule

module sram_piso_cs #(
    parameter WIDTH = 0
)(
    input wire clk,
    input wire rstn,
    input wire shift,
    input wire[WIDTH-1:0] data_in,
    input wire in_valid,
    output reg in_ready,
    output reg pin_io
);
    reg[WIDTH-1:0] data_q;
    reg[WIDTH-1:0] data_nxt;
    
    always_comb
        if (in_valid & in_ready)
            data_nxt = data_in;
        else
            data_nxt = data_q - (shift && !in_ready);
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            pin_io <= 1'b1;
            in_ready <= 1'b0;
        end else begin
            pin_io <= data_nxt == 0;
            in_ready <= data_nxt == 0;
        end
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            data_q <= '0;
        else
            data_q <= data_nxt;
        
endmodule

module sram_controller #(
    parameter SECTOR_BUS_WIDTH = 8,
    parameter BURST_BYTES = 1
)(
    input wire clk,
    input wire rstn,

    output reg sram_sck,
    
    output wire sram_cs,
    inout wire sram_hold,
    inout wire sram_mosi,
    inout wire sram_dq2,
    inout wire sram_miso,
    
    input wire[31:0] in_addr,
    input wire in_wren,
    input wire[8*BURST_BYTES-1:0] in_wdata,
    input wire in_valid,
    output wire in_ready,

    output reg word_out_valid,
    output wire[SECTOR_BUS_WIDTH-1:0] word_out
);
	wire sram_output_capture;
    localparam IDLE_CYCLES = 100;

    localparam MAX_LENGTH = 32+8*BURST_BYTES;
    wire[MAX_LENGTH-1:0] mosi_out;
    wire[MAX_LENGTH-1:0] hold_out;
    wire[MAX_LENGTH-1:0] miso_out;
    wire[MAX_LENGTH-1:0] dq2_out;
    
    reg[MAX_LENGTH-1:0] mosi_in;
    reg[MAX_LENGTH-1:0] mosi_oen;
    reg[MAX_LENGTH-1:0] hold_in;
    reg[MAX_LENGTH-1:0] hold_oen;
    reg[MAX_LENGTH-1:0] miso_in;
    reg[MAX_LENGTH-1:0] miso_oen;
    reg[MAX_LENGTH-1:0] dq2_in;
    reg[MAX_LENGTH-1:0] dq2_oen;
    
    reg sram_req;
    reg[15:0] num_clocks;
    
    
    typedef enum {
        SRAM_POWERUP,
        SRAM_RESET,
        SRAM_CONFIG,
        SRAM_IDLE,
        SRAM_WRITE,
        SRAM_READ
    } SRAM_STATE;
    reg[15:0] state_ctr;
    SRAM_STATE state;
    SRAM_STATE state_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            state <= SRAM_POWERUP;
        else
            state <= state_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            state_ctr <= '0;
        else
            state_ctr <= state != state_nxt ? '0 : state_ctr + 1'b1;

    wire[32+8*BURST_BYTES-1:0] mosi_flatten;
    assign mosi_flatten[0+:8] = state == SRAM_WRITE ? 8'h02 : 8'h0B;
    assign mosi_flatten[8+:8] = in_addr_q[16+:8];
    assign mosi_flatten[16+:8] =in_addr_q[8+:8];
    assign mosi_flatten[24+:8] = in_addr_q[0+:8];
    for (genvar i = 0; i < BURST_BYTES; ++i) begin
        assign mosi_flatten[(32+8*i)+:8] = in_wdata_q[8*i+:8];
    end
    
    
    reg[15:0] read_count_down;
    reg[15:0] read_count_req;
    always_comb begin
        mosi_oen = '0;
        hold_oen = '0;
        miso_oen = '0;
        dq2_oen = '0;
        mosi_in = '0;
        hold_in = '0;
        miso_in = '0;
        dq2_in = '0;
        sram_req = 1'b0;
        num_clocks = '0;
        read_count_down = '1;
        read_count_req = 0;
        case (state)
            SRAM_RESET: begin
                sram_req = 1'b1;
                num_clocks = 8;
                hold_oen = '1;
                hold_in  = '1;
                mosi_oen = '1; 
                mosi_in  = '1;
                dq2_oen = '1;
                dq2_in = '1;
                miso_oen = '1;
                miso_in = '1;

            end
            SRAM_CONFIG: begin
                sram_req = 1'b1;
                num_clocks = 8;
                hold_oen = 8'hFF;
                hold_in  = 8'hFF;
                mosi_oen = 8'hFF;
                mosi_in  = 8'h3B;
                dq2_oen = 8'hFF;
                dq2_in = 8'hFF;
                miso_oen = '0;
                miso_in = '0;
                
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
            SRAM_WRITE: begin
//                sram_req = 1'b1;
//                num_clocks = MAX_LENGTH/4;
//                for (int i = 0; i < 32+8*BURST_BYTES; i += 4) begin
//                    mosi_oen[(i/4)^7] = 1'b1;
//                    miso_oen[(i/4)^7] = 1'b1;
//                    dq2_oen[(i/4)^7] = 1'b1;
//                    hold_oen[(i/4)^7] = 1'b1;

//                    mosi_in[(i/4)^7] = mosi_flatten[i+0];
//                    miso_in[(i/4)^7] = mosi_flatten[i+1];
//                    dq2_in[(i/4)^7]  = mosi_flatten[i+2]; 
//                    hold_in[(i/4)^7] = mosi_flatten[i+3];
//                end

                sram_req = 1'b1;
                num_clocks = MAX_LENGTH/2;
                dq2_oen = '1;
                dq2_in = '1;
                hold_oen = '1;
                hold_in = '1;
                for (int i = 0; i < 8*BURST_BYTES+32; i+=2) begin
                    mosi_oen[(i/2)^4] = 1'b1;
                    miso_oen[(i/2)^4] = 1'b1;
                    mosi_in[(i/2)^4] = mosi_flatten[i+0];
                    miso_in[(i/2)^4] = mosi_flatten[i+1]; 
                end
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
            SRAM_READ: begin
                sram_req = 1'b1;
//                num_clocks = MAX_LENGTH/4+2-1;
//                for (int i = 0; i < 32; i += 4) begin
//                    mosi_oen[(i/4)^7] = 1'b1;
//                    miso_oen[(i/4)^7] = 1'b1;
//                    dq2_oen[(i/4)^7] = 1'b1;
//                    hold_oen[(i/4)^7] = 1'b1;

//                    mosi_in[(i/4)^7] = mosi_flatten[i+0];
//                    miso_in[(i/4)^7] = mosi_flatten[i+1];
//                    dq2_in[(i/4)^7]  = mosi_flatten[i+2]; 
//                    hold_in[(i/4)^7] = mosi_flatten[i+3];
//                end
//                read_count_down = 10;
//                read_count_req = 8*BURST_BYTES/4;



                num_clocks = MAX_LENGTH/2+24/2-1;
                dq2_oen = '1;
                dq2_in = '1;
                hold_oen = '1;
                hold_in = '1;
                for (int i = 0; i < 32; i+=2) begin
                    mosi_oen[(i/2)^4] = 1'b1;
                    miso_oen[(i/2)^4] = 1'b1;
                    mosi_in[(i/2)^4] = mosi_flatten[i+0];
                    miso_in[(i/2)^4] = mosi_flatten[i+1]; 
                end
                read_count_down = 32/2+24/2;
                read_count_req = 8*BURST_BYTES/2;
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
        endcase
    
    end
    wire sram_piso_ready;
    
    reg[7:0] clk_ctr;
    localparam CLK_DIV = 2;
    wire[7:0] clk_rising = CLK_DIV/2;
    wire[7:0] clk_limit = CLK_DIV-1;
    wire[7:0] clk_ctr_nxt = clk_ctr == clk_limit ? '0 : clk_ctr + 1'b1;
    reg clk_out;
    wire clk_fall_nxt = clk_ctr_nxt == 0;
    wire clk_rise_nxt = clk_ctr_nxt == clk_rising;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            clk_ctr <= '0;
        else
            clk_ctr <= clk_ctr_nxt;
    
    reg[15:0] clk_gate_cnt;
    wire sram_cs_commit = sram_req && clk_fall_nxt;
    wire sram_op_commit = sram_cs_commit && sram_piso_ready;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            clk_gate_cnt <= 0;
        else
            clk_gate_cnt <= clk_gate_cnt + (sram_op_commit ? num_clocks : 0) - (clk_fall_nxt && clk_gate_cnt != 0);
    
    reg[15:0] read_out_ctr;
    reg[15:0] read_count;
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            read_out_ctr <= '1;
            read_count <= '0;
        end
        else if (sram_op_commit) begin
            read_out_ctr <= read_count_down;
            read_count <= read_count_req;
        end
        else if (sram_piso_ready) begin
            read_out_ctr <= '1;
            read_count <= 0;
        end
        else if (clk_rise_nxt) begin
            read_out_ctr <= read_out_ctr - (read_out_ctr != 0);
            read_count <= read_count - sram_output_capture;
        end
        
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            sram_sck <= 1'b1;
        else
            sram_sck <= clk_ctr_nxt >= clk_rising && clk_gate_cnt;
    
    assign sram_output_capture = !read_out_ctr && clk_rise_nxt && read_count;
    reg sram_output_capture_q;
    reg sram_output_capture_qq;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            {sram_output_capture_q, sram_output_capture_qq} <= 2'b00;
        else
            {sram_output_capture_q, sram_output_capture_qq} <= {sram_output_capture, sram_output_capture_q};
            
            
    always_comb begin
        state_nxt = state;
        case (state)
            SRAM_POWERUP: if (state_ctr == IDLE_CYCLES) state_nxt = SRAM_RESET;
            SRAM_RESET: if (sram_op_commit) state_nxt = SRAM_CONFIG;
            SRAM_CONFIG: if (sram_op_commit) state_nxt = SRAM_IDLE;
            SRAM_IDLE: if (in_valid) state_nxt = in_wren ? SRAM_WRITE : SRAM_READ;
            SRAM_WRITE: if (sram_op_commit) state_nxt = SRAM_IDLE;
            SRAM_READ: if (sram_op_commit) state_nxt = SRAM_IDLE;
        endcase
    end
    reg[31:0] in_addr_q;
    reg[8*BURST_BYTES-1:0] in_wdata_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            in_addr_q <= '0;
            in_wdata_q <= '0;
        end else if (in_valid & in_ready) begin
            in_addr_q <= in_addr;
            in_wdata_q <= in_wdata;
        end
    assign in_ready = state == SRAM_IDLE;
    reg read_out_valid_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            read_out_valid_q <= 1'b0;
        else
            read_out_valid_q <= clk_rise_nxt && read_count == 1;
            
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) mosi_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(mosi_in),
        .oen_in(mosi_oen),
        .capture(sram_output_capture_qq),
        .in_valid(sram_op_commit),
        .pin_io(sram_mosi),
        .data_out(mosi_out)
    );
    
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) hold_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(hold_in),
        .oen_in(hold_oen),
        .capture(sram_output_capture_qq),
        .in_valid(sram_op_commit),
        .pin_io(sram_hold),
        .data_out(hold_out)
        
    );

    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) dq2_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(dq2_in),
        .oen_in(dq2_oen),
        .capture(sram_output_capture_qq),
        .in_valid(sram_op_commit),
        .pin_io(sram_dq2),
        .data_out(dq2_out)
        
    ); 
    
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) miso_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(miso_in),
        .oen_in(miso_oen),
        .capture(sram_output_capture_qq),
        .in_valid(sram_op_commit),
        .pin_io(sram_miso),
        .data_out(miso_out)
        
    ); 
    sram_piso_cs #(.WIDTH(16)) cs_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(num_clocks+1),
        .in_valid(sram_cs_commit),
        .in_ready(sram_piso_ready),
        .pin_io(sram_cs)
    );

    reg[$clog2(SECTOR_BUS_WIDTH/2)-1:0] word_ctr;
    reg[$clog2(SECTOR_BUS_WIDTH/2)-1:0] word_ctr_nxt;
    always_comb
        if (sram_output_capture_qq)
            word_ctr_nxt = word_ctr < SECTOR_BUS_WIDTH/2-1 ? word_ctr + 1'b1 : '0;
        else
            word_ctr_nxt = word_ctr;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            word_ctr <= '0;
        else
            word_ctr <= word_ctr_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            word_out_valid <= 1'b0;
        else
            word_out_valid <= word_ctr == SECTOR_BUS_WIDTH/2-1 && sram_output_capture_qq;
    assign word_out = {miso_out[3], mosi_out[3], miso_out[2], mosi_out[2], 
                       miso_out[1], mosi_out[1], miso_out[0], mosi_out[0]};
    
    
endmodule 



/*
module sram_controller #(
    parameter SECTOR_BUS_WIDTH = 8,
    parameter BURST_BYTES = 1
)(
    input wire clk,
    input wire rstn,

    output reg sram_sck,
    
    output wire sram_cs,
    inout wire sram_hold,
    inout wire sram_mosi,
    inout wire sram_dq2,
    inout wire sram_miso,
    
    input wire[31:0] in_addr,
    input wire in_wren,
    input wire[8*BURST_BYTES-1:0] in_wdata,
    input wire in_valid,
    output wire in_ready,

    output reg word_out_valid,
    output wire[SECTOR_BUS_WIDTH-1:0] word_out
);

    localparam IDLE_CYCLES = 100;

    localparam MAX_LENGTH = 32+8*BURST_BYTES;
    wire[MAX_LENGTH-1:0] mosi_out;
    wire[MAX_LENGTH-1:0] hold_out;
    wire[MAX_LENGTH-1:0] miso_out;
    wire[MAX_LENGTH-1:0] dq2_out;
    
    reg[MAX_LENGTH-1:0] mosi_in;
    reg[MAX_LENGTH-1:0] mosi_oen;
    reg[MAX_LENGTH-1:0] hold_in;
    reg[MAX_LENGTH-1:0] hold_oen;
    reg[MAX_LENGTH-1:0] miso_in;
    reg[MAX_LENGTH-1:0] miso_oen;
    reg[MAX_LENGTH-1:0] dq2_in;
    reg[MAX_LENGTH-1:0] dq2_oen;
    
    reg sram_req;
    reg[15:0] num_clocks;
    
    
    typedef enum {
        SRAM_POWERUP,
        SRAM_RESET,
        SRAM_CONFIG,
        SRAM_IDLE,
        SRAM_WRITE,
        SRAM_READ
    } SRAM_STATE;
    reg[15:0] state_ctr;
    SRAM_STATE state;
    SRAM_STATE state_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            state <= SRAM_POWERUP;
        else
            state <= state_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            state_ctr <= '0;
        else
            state_ctr <= state != state_nxt ? '0 : state_ctr + 1'b1;

    wire[32+8*BURST_BYTES-1:0] mosi_flatten;
    assign mosi_flatten[0+:8] = state == SRAM_WRITE ? 8'h02 : 8'h03;
    assign mosi_flatten[8+:8] = in_addr_q[16+:8];
    assign mosi_flatten[16+:8] =in_addr_q[8+:8];
    assign mosi_flatten[24+:8] = in_addr_q[0+:8];
    for (genvar i = 0; i < BURST_BYTES; ++i) begin
        assign mosi_flatten[(32+8*i)+:8] = in_wdata_q[8*i+:8];
    end
    
    
    reg[15:0] read_count_down;
    reg[15:0] read_count_req;
    always_comb begin
        mosi_oen = '0;
        hold_oen = '0;
        miso_oen = '0;
        dq2_oen = '0;
        mosi_in = '0;
        hold_in = '0;
        miso_in = '0;
        dq2_in = '0;
        sram_req = 1'b0;
        num_clocks = '0;
        read_count_down = '1;
        read_count_req = 0;
        case (state)
            SRAM_RESET: begin
                sram_req = 1'b1;
                num_clocks = 8;
                hold_oen = '1;
                hold_in  = '1;
                mosi_oen = '1; 
                mosi_in  = '1;
                dq2_oen = '1;
                dq2_in = '1;
                miso_oen = '1;
                miso_in = '1;

            end
            SRAM_CONFIG: begin
                sram_req = 1'b1;
                num_clocks = 8;
                hold_oen = '1;
                hold_in  = '1;
                mosi_oen = '1;
                mosi_in  = 8'h38;
                dq2_oen = '1;
                dq2_in = '1;
                miso_oen = '1;
                miso_in = '1;
                
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
            SRAM_WRITE: begin

                sram_req = 1'b1;
                num_clocks = MAX_LENGTH/4;
  
                for (int i = 0; i < 8*BURST_BYTES+32; i+=4) begin
                    mosi_oen[(i/4)^6] = 1'b1;
                    miso_oen[(i/4)^6] = 1'b1;
                    dq2_oen[(i/4)^6] = 1'b1;
                    hold_oen[(i/4)^6] = 1'b1;
                    mosi_in[(i/4)^6] = mosi_flatten[i+0];
                    miso_in[(i/4)^6] = mosi_flatten[i+1]; 
                    dq2_in[(i/4)^6] = mosi_flatten[i+2];
                    hold_in[(i/4)^6] = mosi_flatten[i+3];
                end
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
            SRAM_READ: begin
                sram_req = 1'b1;
                num_clocks = MAX_LENGTH/4+8/4-1;
                for (int i = 0; i < 32; i+=4) begin
                    mosi_oen[(i/4)^6] = 1'b1;
                    miso_oen[(i/4)^6] = 1'b1;
                    dq2_oen[(i/4)^6] = 1'b1;
                    hold_oen[(i/4)^6] = 1'b1;
                    mosi_in[(i/4)^6] = mosi_flatten[i+0];
                    miso_in[(i/4)^6] = mosi_flatten[i+1]; 
                    dq2_in[(i/4)^6] = mosi_flatten[i+2];
                    hold_in[(i/4)^6] = mosi_flatten[i+3];
                end
                read_count_down = 32/4+8/4;
                read_count_req = 8*BURST_BYTES/4;
                hold_oen = {<<8{hold_oen}};
                hold_in  = {<<8{hold_in}};
                mosi_oen = {<<8{mosi_oen}};
                mosi_in  = {<<8{mosi_in}};
                dq2_oen = {<<8{dq2_oen}};
                dq2_in = {<<8{dq2_in}};
                miso_oen = {<<8{miso_oen}};
                miso_in = {<<8{miso_in}};
            end
        endcase
    
    end
    wire sram_piso_ready;
    
    reg[7:0] clk_ctr;
    localparam CLK_DIV = 4;
    wire[7:0] clk_rising = CLK_DIV/2;
    wire[7:0] clk_limit = CLK_DIV-1;
    wire[7:0] clk_ctr_nxt = clk_ctr == clk_limit ? '0 : clk_ctr + 1'b1;
    reg clk_out;
    wire clk_fall_nxt = clk_ctr_nxt == 0;
    wire clk_rise_nxt = clk_ctr_nxt == clk_rising;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            clk_ctr <= '0;
        else
            clk_ctr <= clk_ctr_nxt;
    
    reg[15:0] clk_gate_cnt = 0;
    wire sram_cs_commit = sram_req && clk_fall_nxt;
    wire sram_op_commit = sram_cs_commit && sram_piso_ready;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            clk_gate_cnt <= 0;
        else
            clk_gate_cnt <= clk_gate_cnt + (sram_op_commit ? num_clocks : 0) - (clk_fall_nxt && clk_gate_cnt != 0);
    
    reg[15:0] read_out_ctr;
    reg[15:0] read_count;
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            read_out_ctr <= '1;
            read_count <= '0;
        end
        else if (sram_op_commit) begin
            read_out_ctr <= read_count_down;
            read_count <= read_count_req;
        end
        else if (sram_piso_ready) begin
            read_out_ctr <= '1;
            read_count <= 0;
        end
        else if (clk_rise_nxt) begin
            read_out_ctr <= read_out_ctr - (read_out_ctr != 0);
            read_count <= read_count - sram_output_capture;
        end
        
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            sram_sck <= 1'b1;
        else
            sram_sck <= clk_ctr_nxt >= clk_rising && clk_gate_cnt;
    
    wire sram_output_capture = !read_out_ctr && clk_rise_nxt && read_count;
            
    always_comb begin
        state_nxt = state;
        case (state)
            SRAM_POWERUP: if (state_ctr == IDLE_CYCLES) state_nxt = SRAM_RESET;
            SRAM_RESET: if (sram_op_commit) state_nxt = SRAM_CONFIG;
            SRAM_CONFIG: if (sram_op_commit) state_nxt = SRAM_IDLE;
            SRAM_IDLE: if (in_valid) state_nxt = in_wren ? SRAM_WRITE : SRAM_READ;
            SRAM_WRITE: if (sram_op_commit) state_nxt = SRAM_IDLE;
            SRAM_READ: if (sram_op_commit) state_nxt = SRAM_IDLE;
        endcase
    end
    reg[31:0] in_addr_q;
    reg[8*BURST_BYTES-1:0] in_wdata_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            in_addr_q <= '0;
            in_wdata_q <= '0;
        end else if (in_valid & in_ready) begin
            in_addr_q <= in_addr;
            in_wdata_q <= in_wdata;
        end
    assign in_ready = state == SRAM_IDLE;
    reg read_out_valid_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            read_out_valid_q <= 1'b0;
        else
            read_out_valid_q <= clk_rise_nxt && read_count == 1;
            
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) mosi_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(mosi_in),
        .oen_in(mosi_oen),
        .capture(sram_output_capture),
        .in_valid(sram_op_commit),
        .pin_io(sram_mosi),
        .data_out(mosi_out)
    );
    
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) hold_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(hold_in),
        .oen_in(hold_oen),
        .capture(sram_output_capture),
        .in_valid(sram_op_commit),
        .pin_io(sram_hold),
        .data_out(hold_out)
        
    );

    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) dq2_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(dq2_in),
        .oen_in(dq2_oen),
        .capture(sram_output_capture),
        .in_valid(sram_op_commit),
        .pin_io(sram_dq2),
        .data_out(dq2_out)
        
    ); 
    
    sram_piso_tristate #(.WIDTH(MAX_LENGTH)) miso_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(miso_in),
        .oen_in(miso_oen),
        .capture(sram_output_capture),
        .in_valid(sram_op_commit),
        .pin_io(sram_miso),
        .data_out(miso_out)
        
    ); 
    sram_piso_cs #(.WIDTH(16)) cs_piso(
        .clk,
        .rstn,
        .shift(clk_fall_nxt),
        .data_in(num_clocks+1'b1),
        .in_valid(sram_cs_commit),
        .in_ready(sram_piso_ready),
        .pin_io(sram_cs)
    );

    reg[$clog2(SECTOR_BUS_WIDTH/4)-1:0] word_ctr;
    reg[$clog2(SECTOR_BUS_WIDTH/4)-1:0] word_ctr_nxt;
    always_comb
        if (sram_output_capture)
            word_ctr_nxt = word_ctr < SECTOR_BUS_WIDTH/4-1 ? word_ctr + 1'b1 : '0;
        else
            word_ctr_nxt = word_ctr;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            word_ctr <= '0;
        else
            word_ctr <= word_ctr_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            word_out_valid <= 1'b0;
        else
            word_out_valid <= word_ctr == SECTOR_BUS_WIDTH/4-1 && sram_output_capture;
    assign word_out = {hold_out[1], dq2_out[1], miso_out[1], mosi_out[1], hold_out[0], dq2_out[0], miso_out[0], mosi_out[0]};
    
    
endmodule*/
