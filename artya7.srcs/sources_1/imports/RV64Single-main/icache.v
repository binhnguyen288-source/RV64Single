`default_nettype none

module Icache #(
    parameter CACHE_LINE = 0,
    parameter CACHE_SIZE = 64,
    parameter MEM_BUS_WIDTH = 32,
    parameter SECTOR_BUS_WIDTH = 0
)(
    input wire clk,
    input wire rstn,
    input wire mem_req,
    input wire[31:0] mem_addr_w,
    input wire icache_flush,
    output wire mem_gnt,
    output reg mem_rvalid,
    output wire[MEM_BUS_WIDTH-1:0] mem_rdata,
    
    output wire sector_req,
    output wire[31:0] sector_addr,
    input wire sector_gnt,
    
    
    input wire sector_rvalid,
    input wire[SECTOR_BUS_WIDTH-1:0] sector_rdata
    
    
    );
    
    

    localparam CACHE_NUM_ENTRIES = CACHE_SIZE / CACHE_LINE;
    
    localparam OFFSET_SIZE = $clog2(CACHE_LINE);
    localparam IDX_SIZE = $clog2(CACHE_NUM_ENTRIES);
    localparam TAG_SIZE = 32 - IDX_SIZE - OFFSET_SIZE;
    localparam STATE_CTR_LIMIT = CACHE_LINE * 8 / SECTOR_BUS_WIDTH - 1;

    localparam MEM_SECTOR_BUS_RATIO = MEM_BUS_WIDTH / SECTOR_BUS_WIDTH;
    localparam STATE_CTR_RVALID_IDX = $clog2(MEM_SECTOR_BUS_RATIO);
    localparam MEM_BUS_WIDTH_START = $clog2(MEM_BUS_WIDTH/8);
    
    typedef struct packed {
        logic[TAG_SIZE-1:0] tag;
        logic valid;
    } cache_tag_t;

    cache_tag_t tag_arr[CACHE_NUM_ENTRIES];

    
    wire[IDX_SIZE-1:0] cache_idx_w = mem_addr_w[OFFSET_SIZE+:IDX_SIZE];

    
    wire cache_miss_w = !tag_arr[cache_idx_w].valid || tag_arr[cache_idx_w].tag != mem_addr_w[31-:TAG_SIZE];
    
    typedef enum {
        CACHE_IDLE = 1 << 0,
        CACHE_FETCH_REQ = 1 << 1,
        CACHE_FETCH_DATA = 1 << 2
    } CacheState;
    

    reg[31:0] mem_addr_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            mem_addr_q <= '0;
        else if (mem_req & mem_gnt)
            mem_addr_q <= mem_addr_w;

            
    
    wire[IDX_SIZE-1:0] cache_idx_q = mem_addr_q[OFFSET_SIZE+:IDX_SIZE];
    
    
    
    CacheState cache_state;

    assign sector_req = cache_state == CACHE_FETCH_REQ;
    assign sector_addr = {mem_addr_q[31-:TAG_SIZE], cache_idx_q, {OFFSET_SIZE{1'b0}}};
    localparam STATE_CTR_SIZE = $clog2(STATE_CTR_LIMIT);
    
    reg[STATE_CTR_SIZE-1:0] state_ctr;
    
    reg pending_req;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            pending_req <= 1'b0;
        else if (mem_req & mem_gnt)
            pending_req <= cache_miss_w;
        else if (cache_state == CACHE_IDLE)
            pending_req <= 1'b0;
    
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
            cache_state <= CACHE_IDLE;
            tag_arr <= '{default: '0};
            state_ctr <= '0;
        end else if (icache_flush) begin
            cache_state <= CACHE_IDLE;
            tag_arr <= '{default: '0};
            state_ctr <= '0;
        end else begin
            case (cache_state)
                CACHE_IDLE: 
                    if (mem_req & mem_gnt & cache_miss_w)
                        cache_state <= CACHE_FETCH_REQ;
                CACHE_FETCH_REQ: begin
//                    state_ctr <= '0;
                    if (sector_gnt)
                        cache_state <= CACHE_FETCH_DATA;
                end
                CACHE_FETCH_DATA: if (sector_rvalid) begin
                    state_ctr <= state_ctr + 1'b1;
                    if (state_ctr == STATE_CTR_LIMIT) begin
                        tag_arr[cache_idx_q] <= '{valid: 1'b1, tag: mem_addr_q[31-:TAG_SIZE]};
                        cache_state <= CACHE_IDLE;
                    end
                end
            endcase
        end
    
    always @(posedge clk or negedge rstn)
        if (!rstn)
            mem_rvalid <= 1'b0;
        else 
            mem_rvalid <= mem_req && mem_gnt && ~cache_miss_w || pending_req && cache_state == CACHE_IDLE;
    
    wire[MEM_BUS_WIDTH-1:0] sector_rewire;
    wire sector_sram_wren;
    generate
        if (MEM_BUS_WIDTH > SECTOR_BUS_WIDTH) begin
            reg[MEM_BUS_WIDTH-SECTOR_BUS_WIDTH-1:0] sector_rbuf;
            if (MEM_BUS_WIDTH == 2 * SECTOR_BUS_WIDTH) begin
                always @(posedge clk or negedge rstn)
			if (!rstn)
				sector_rbuf <= '0;
                    else if (sector_rvalid) 
                    if (sector_rvalid) 
                        sector_rbuf <= sector_rdata;
            end else begin
                always @(posedge clk or negedge rstn)
			if (!rstn)
				sector_rbuf <= '0;
                    else if (sector_rvalid) 
                    if (sector_rvalid) 
                        sector_rbuf <= {sector_rdata, sector_rbuf[MEM_BUS_WIDTH-SECTOR_BUS_WIDTH-1:SECTOR_BUS_WIDTH]};
            end
            assign sector_rewire = { sector_rdata, sector_rbuf };
            assign sector_sram_wren = (&state_ctr[0+:STATE_CTR_RVALID_IDX]) && sector_rvalid && cache_state == CACHE_FETCH_DATA;
        end else begin
            assign sector_rewire = sector_rdata;
            assign sector_sram_wren = sector_rvalid && cache_state == CACHE_FETCH_DATA;
        end
    endgenerate
    

    SinglePortMemx32 #(.DEPTH(CACHE_SIZE / 4)) your_instance_name(
        .dout(mem_rdata),
        .clk,
        .ce(sector_sram_wren | (pending_req && cache_state == CACHE_IDLE || mem_req)),
        .wren(sector_sram_wren),
        .wstrobe(4'b1111),
        .addr(sector_sram_wren ? {cache_idx_q, state_ctr[STATE_CTR_SIZE-1:STATE_CTR_RVALID_IDX]} : 
              pending_req ? mem_addr_q[OFFSET_SIZE+IDX_SIZE-1:MEM_BUS_WIDTH_START] : mem_addr_w[OFFSET_SIZE+IDX_SIZE-1:MEM_BUS_WIDTH_START]),
        .din(sector_rewire),
        .rstn
    );

    
    assign mem_gnt = ~pending_req;
    
    
    
    
    
    
endmodule
