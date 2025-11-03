`default_nettype none
module dcache128 #(
    parameter CACHE_LINE = 0,
    parameter CACHE_SIZE = 64,
    parameter MEM_BUS_WIDTH = 64,
    parameter SECTOR_BUS_WIDTH = 0
)(
    input wire clk,
    input wire rstn,
    input wire mem_req,
    input wire[31:0] mem_addr,
    input wire mem_wren,
    input wire[MEM_BUS_WIDTH-1:0] mem_wdata,
    input wire[7:0] mem_wstrobe,
    
    output wire mem_gnt,
    output reg mem_rvalid,
    output wire[MEM_BUS_WIDTH-1:0] mem_rdata,
    
    output wire sector_req,
    output wire sector_wren,
    output wire[31:0] sector_addr,
    input wire sector_gnt,
    output reg sector_wvalid,
    output wire[SECTOR_BUS_WIDTH-1:0] sector_wdata,
    input wire sector_wready,
    
    
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
    
    localparam CACHE_LINE_ADDR_SIZE = 32 - OFFSET_SIZE;
    typedef struct packed {
        logic[TAG_SIZE-1:0] tag;
        logic valid;
        logic dirty;
    } cache_tag_t;
    cache_tag_t tag_arr[CACHE_NUM_ENTRIES];

    
    wire[IDX_SIZE-1:0] cache_idx = mem_addr[OFFSET_SIZE+:IDX_SIZE];
    
    wire cache_miss = !tag_arr[cache_idx].valid || tag_arr[cache_idx].tag != mem_addr[31-:TAG_SIZE];
    
    typedef enum {
        CACHE_IDLE,
        CACHE_WRITEBACK_REQ,
        CACHE_WRITEBACK_DATA,
        CACHE_FETCH_REQ,
        CACHE_FETCH_DATA
    } CacheState;
    
    
    
    wire[31:0] cache_wb_addr = {tag_arr[cache_idx].tag, cache_idx, {OFFSET_SIZE{1'b0}}};
    wire[31:0] cache_fetch_addr = {mem_addr[31-:TAG_SIZE], cache_idx, {OFFSET_SIZE{1'b0}}};
    
    
    
    CacheState cache_state;

    assign sector_req = cache_state == CACHE_WRITEBACK_REQ || cache_state == CACHE_FETCH_REQ;
    assign sector_wren = cache_state == CACHE_WRITEBACK_REQ;
    assign sector_addr = cache_state == CACHE_WRITEBACK_REQ ? cache_wb_addr : cache_fetch_addr;

    localparam STATE_CTR_SIZE = $clog2(STATE_CTR_LIMIT);
    
    reg[STATE_CTR_SIZE-1:0] state_ctr;
    reg[STATE_CTR_SIZE-1:0] state_ctr_nxt;
    
    
    CacheState cache_state_nxt;
    always_comb begin
        cache_state_nxt = cache_state;
        state_ctr_nxt = state_ctr;
        case (cache_state)
            CACHE_IDLE: if (mem_req && cache_miss) begin
                state_ctr_nxt = 0;
                cache_state_nxt = (tag_arr[cache_idx].valid && tag_arr[cache_idx].dirty) ? CACHE_WRITEBACK_REQ : CACHE_FETCH_REQ;
            end
            CACHE_WRITEBACK_REQ: if (sector_gnt) begin
                state_ctr_nxt = 0;
                cache_state_nxt = CACHE_WRITEBACK_DATA;
            end
            CACHE_WRITEBACK_DATA: if (sector_wready)  begin
                state_ctr_nxt = state_ctr + 1'b1;
                if (state_ctr == STATE_CTR_LIMIT)
                    cache_state_nxt = CACHE_FETCH_REQ;
            end
            CACHE_FETCH_REQ: if (sector_gnt) begin
                state_ctr_nxt = 0;
                cache_state_nxt = CACHE_FETCH_DATA;
            end
            CACHE_FETCH_DATA: if (sector_rvalid) begin
                state_ctr_nxt = state_ctr + 1'b1;
                if (state_ctr == STATE_CTR_LIMIT)
                    cache_state_nxt = CACHE_IDLE;
            end
        endcase
    end
    
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
            tag_arr <= '{default: '0};
            cache_state <= CACHE_IDLE;
            state_ctr <= '0;
            sector_wvalid <= 1'b0;
            
        end
        else begin
            cache_state <= cache_state_nxt;
            state_ctr <= state_ctr_nxt;
            sector_wvalid <= cache_state_nxt == CACHE_WRITEBACK_DATA;
            case (cache_state)
                CACHE_IDLE: if (mem_req && !cache_miss & mem_wren) begin
                    tag_arr[cache_idx].dirty <= 1'b1;
                end
                CACHE_WRITEBACK_REQ: begin end
                CACHE_WRITEBACK_DATA: begin end
                CACHE_FETCH_REQ: begin end
                CACHE_FETCH_DATA: if (sector_rvalid) begin
                    if (state_ctr == STATE_CTR_LIMIT) begin
                        tag_arr[cache_idx] <= '{tag: mem_addr[31-:TAG_SIZE], valid: 1'b1, dirty: 1'b0};
                    end
                end
            endcase
        end
    
    always @(posedge clk or negedge rstn)
        if (!rstn)
            mem_rvalid <= 1'b0;
        else
            mem_rvalid <= mem_gnt & mem_req;


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
                        sector_rbuf <= sector_rdata;
            end else begin
                always @(posedge clk or negedge rstn)
			if (!rstn)
				sector_rbuf <= '0;
                    else if (sector_rvalid) 
                        sector_rbuf <= {sector_rdata, sector_rbuf[MEM_BUS_WIDTH-SECTOR_BUS_WIDTH-1:SECTOR_BUS_WIDTH]};
            end
            assign sector_rewire = { sector_rdata, sector_rbuf };
            assign sector_sram_wren = (&state_ctr[0+:STATE_CTR_RVALID_IDX]) && sector_rvalid && cache_state == CACHE_FETCH_DATA;
        end else begin
            assign sector_rewire = sector_rdata;
            assign sector_sram_wren = sector_rvalid && cache_state == CACHE_FETCH_DATA;
        end
    endgenerate

    wire[MEM_BUS_WIDTH-1:0] sram_dout;
    assign sector_wdata = sram_dout[(state_ctr[0+:STATE_CTR_RVALID_IDX]*SECTOR_BUS_WIDTH)+:SECTOR_BUS_WIDTH];
    
    wire cpu_or_ext = cache_state == CACHE_IDLE;
    wire[63:0] dout;
    assign mem_rdata = dout;
    assign sram_dout = dout;
    SinglePortMemx64 #(.DEPTH(CACHE_SIZE / 8)) your_instance_name(
        .dout,
        .clk,
        .ce((sector_rvalid && cache_state == CACHE_FETCH_DATA) || 
               (cache_state_nxt == CACHE_WRITEBACK_DATA) || (mem_gnt & mem_req)),
        .wren(cpu_or_ext ? mem_wren : sector_sram_wren),
        .wstrobe(cpu_or_ext ? mem_wstrobe : 8'hFF),
        .addr(cpu_or_ext ? {cache_idx, mem_addr[OFFSET_SIZE-1:MEM_BUS_WIDTH_START]} : {cache_idx, cache_state == CACHE_FETCH_DATA ? state_ctr[STATE_CTR_SIZE-1:STATE_CTR_RVALID_IDX] : 
                                                           state_ctr_nxt[STATE_CTR_SIZE-1:STATE_CTR_RVALID_IDX]}),
        .din(cpu_or_ext ? mem_wdata : sector_rewire),
        .rstn
    );
    
    assign mem_gnt = !cache_miss && cache_state == CACHE_IDLE;
    
    
    
    
    
    
endmodule

