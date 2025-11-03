`default_nettype none


module sync_fifo #(
    parameter WIDTH = 0,
    parameter DEPTH = 8
)(
    input wire clk,
    input wire rstn,
    
    input wire wvalid,
    input wire[WIDTH-1:0] wdata,
    output wire wready,
    
    output wire rvalid,
    output wire[WIDTH-1:0] rdata,
    input wire rready
);
    localparam PTR_W = $clog2(DEPTH);
    
    reg[WIDTH-1:0] mem[DEPTH];
    reg[PTR_W:0] rdptr;
    reg[PTR_W:0] wrptr;
    
    wire internal_rvalid = rdptr != wrptr;
    
    
    wire[PTR_W:0] rdptr_nxt = rdptr + (internal_rvalid & (~rvalid | rready));
    wire[PTR_W:0] wrptr_nxt = wrptr + (wready & wvalid);
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            wrptr <= 0;
        else
            wrptr <= wrptr_nxt;
    
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rdptr <= 0;
        else
            rdptr <= rdptr_nxt;        
            
            
    reg wready_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            wready_q <= 1'b0;
        else
            wready_q <= {~wrptr_nxt[PTR_W], wrptr_nxt[PTR_W-1:0]} != rdptr_nxt;
    assign wready = wready_q;
    for (genvar i = 0; i < DEPTH; ++i) begin
    	always_ff @(posedge clk or negedge rstn)
		if (!rstn)
			mem[i] <= '0;
        	else if (wready && wvalid && i == wrptr[PTR_W-1:0])
            		mem[i] <= wdata;
    end
            
    reg rvalid_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid_q <= 1'b0;
        else if (~rvalid | rready)
            rvalid_q <= internal_rvalid;
    assign rvalid = rvalid_q;
    reg[WIDTH-1:0] rdata_q;
    always_ff @(posedge clk or negedge rstn)
    	if (!rstn)
		rdata_q <= '0;
        else if (~rvalid | rready)
            rdata_q <= mem[rdptr[PTR_W-1:0]];
    assign rdata = rdata_q;
endmodule


module SIFIVE_UART(
    input wire clk,
    input wire rstn,

    input wire[11:0] addr,
    input wire req,
    input wire wren,
    output wire gnt,
    input wire[63:0] wdata,
    input wire[7:0] wstrb,
    output reg[63:0] rdata,
    output reg rvalid,
    output wire uart_tx,
    input wire uart_rx
);

    localparam COUNTER_W = 16;
    reg[COUNTER_W-1:0] UART_BIT_PERIOD;
    wire[COUNTER_W-1:0] UART_SAMPLE_THRESHOLD = UART_BIT_PERIOD[COUNTER_W-1:1];
    
    reg sifive_txen;
    reg sifive_rxen;
    wire uart_tx_full;
    wire[7:0] uart_rx_byte;
    wire uart_rx_empty;
    
    
    wire[31:0] sifive_txdata = {uart_tx_full, 31'h0};
    wire[31:0] sifive_rxdata = {uart_rx_empty, 23'h0, uart_rx_byte};
    wire[31:0] sifive_txctrl = sifive_txen;
    wire[31:0] sifive_rxctrl = sifive_rxen;
    wire[31:0] sifive_baudrate = UART_BIT_PERIOD;
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rdata <= '0;
        else if (req & gnt)
            case ({addr[11:2], 2'b00})
                32'h0:   rdata <= {2{sifive_txdata}};
                32'h4:   rdata <= {2{sifive_rxdata}};
                32'h8:   rdata <= {2{sifive_txctrl}};
                32'hc:   rdata <= {2{sifive_rxctrl}};
                32'h18:  rdata <= {2{sifive_baudrate}};
                default: rdata <= '0;
            endcase
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            sifive_txen <= 1'b0;
            sifive_rxen <= 1'b0;
            UART_BIT_PERIOD <= 433;
        end else if (req & gnt & wren)
            case ({addr[11:2], 2'b00})
                32'h8: sifive_txen <= wdata[0];
                32'hc: sifive_rxen <= wdata[32];
                32'h18: UART_BIT_PERIOD <= wdata[COUNTER_W-1:0];
            endcase
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            rvalid <= 1'b0;
        else
            rvalid <= req & gnt;
            
    assign gnt = 1'b1;
    
    wire tx_read_en;
    wire[7:0] tx_data;
    wire tx_fifo_rvalid;
    wire tx_fifo_wready;
    assign uart_tx_full = ~tx_fifo_wready;
    sync_fifo #(
        .WIDTH(8)
    ) phy_tx_fifo(
		.wdata(wdata[7:0]), //input [7:0] Data
		.clk(clk), //input Clk
		.wvalid(req && gnt && wren && {addr[11:2], 2'b00} == 32'h0), //input WrEn
		.rready(tx_read_en), //input RdEn
		.rstn(sifive_txen), //input Reset
		.rdata(tx_data), //output [7:0] Q
		.rvalid(tx_fifo_rvalid), //output Empty
		.wready(tx_fifo_wready) //output Full
	);
    

    typedef enum {
        TX_WAIT,
        TX_INIT,
        TX_PAYLOAD0,
        TX_PAYLOAD1,
        TX_PAYLOAD2,
        TX_PAYLOAD3,
        TX_PAYLOAD4,
        TX_PAYLOAD5,
        TX_PAYLOAD6,
        TX_PAYLOAD7,
        TX_END
    } TX_STATE;
    
    reg[COUNTER_W-1:0] tx_ctr;
    TX_STATE uart_tx_state;
    TX_STATE uart_tx_nextstate;
    assign tx_read_en = (uart_tx_state == TX_WAIT) && tx_fifo_rvalid;
    reg[7:0] transmit_frame;
    always_comb begin
        case (uart_tx_state)
            TX_WAIT: uart_tx_nextstate = tx_fifo_rvalid ? TX_INIT : TX_WAIT;
            TX_INIT: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD0 : TX_INIT;
            TX_PAYLOAD0: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD1 : TX_PAYLOAD0;
            TX_PAYLOAD1: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD2 : TX_PAYLOAD1;
            TX_PAYLOAD2: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD3 : TX_PAYLOAD2;
            TX_PAYLOAD3: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD4 : TX_PAYLOAD3;
            TX_PAYLOAD4: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD5 : TX_PAYLOAD4;
            TX_PAYLOAD5: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD6 : TX_PAYLOAD5;
            TX_PAYLOAD6: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_PAYLOAD7 : TX_PAYLOAD6;
            TX_PAYLOAD7: uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_END      : TX_PAYLOAD7;
            TX_END:      uart_tx_nextstate = tx_ctr == UART_BIT_PERIOD ? TX_WAIT     : TX_END;
            default: uart_tx_nextstate = TX_WAIT;
        endcase
    end
    always_ff @(posedge clk or negedge sifive_txen)
        if (!sifive_txen)
            transmit_frame <= '0;
        else if (tx_read_en)
            transmit_frame <= tx_data;
    always_ff @(posedge clk or negedge sifive_txen)
        if (!sifive_txen)
            tx_ctr <= '0;
        else
            tx_ctr <= uart_tx_nextstate == uart_tx_state ? tx_ctr + 1'b1 : '0;

    always_ff @(posedge clk or negedge sifive_txen)
        if (!sifive_txen)
            uart_tx_state <= TX_WAIT;
        else
            uart_tx_state <= uart_tx_nextstate;
    
    reg uart_tx_q;
    always_ff @(posedge clk or negedge sifive_txen)
        if (!sifive_txen)
            uart_tx_q <= 1'b1;
        else
            case (uart_tx_state)
                TX_WAIT: uart_tx_q <= 1'b1;
                TX_INIT: uart_tx_q <= 1'b0;
                TX_PAYLOAD0: uart_tx_q <= transmit_frame[0];
                TX_PAYLOAD1: uart_tx_q <= transmit_frame[1];
                TX_PAYLOAD2: uart_tx_q <= transmit_frame[2];
                TX_PAYLOAD3: uart_tx_q <= transmit_frame[3];
                TX_PAYLOAD4: uart_tx_q <= transmit_frame[4];
                TX_PAYLOAD5: uart_tx_q <= transmit_frame[5];
                TX_PAYLOAD6: uart_tx_q <= transmit_frame[6];
                TX_PAYLOAD7: uart_tx_q <= transmit_frame[7];
                TX_END: uart_tx_q <= 1'b1;
                default: uart_tx_q <= 1'b1;
            endcase
    reg uart_tx_qq;
    always_ff @(posedge clk or negedge sifive_txen)
        if (!sifive_txen)
            uart_tx_qq <= 1'b1;
        else
            uart_tx_qq <= uart_tx_q;
    assign uart_tx = uart_tx_qq;
    
    
    reg[2:0] uart_cdc_sync_q;

    always_ff @(posedge clk or negedge sifive_rxen)
        if (!sifive_rxen)
            uart_cdc_sync_q <= '1;
        else
            uart_cdc_sync_q <= {uart_rx, uart_cdc_sync_q[2:1]};

    wire uart_rx_q = uart_cdc_sync_q[0];

    
    typedef enum {
        RX_INIT,
        RX_PAYLOAD0,
        RX_PAYLOAD1,
        RX_PAYLOAD2,
        RX_PAYLOAD3,
        RX_PAYLOAD4,
        RX_PAYLOAD5,
        RX_PAYLOAD6,
        RX_PAYLOAD7,
        RX_END
    } RX_STATE;
    RX_STATE uart_rx_state;
    
    reg[COUNTER_W-1:0] rx_ctr;
    
    RX_STATE uart_rx_nextstate;
    always_comb
        case (uart_rx_state)
            RX_INIT: uart_rx_nextstate = rx_ctr == UART_SAMPLE_THRESHOLD ? RX_PAYLOAD0 : RX_INIT;
            RX_PAYLOAD0: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD1 : RX_PAYLOAD0;
            RX_PAYLOAD1: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD2 : RX_PAYLOAD1;
            RX_PAYLOAD2: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD3 : RX_PAYLOAD2;
            RX_PAYLOAD3: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD4 : RX_PAYLOAD3;
            RX_PAYLOAD4: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD5 : RX_PAYLOAD4;
            RX_PAYLOAD5: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD6 : RX_PAYLOAD5;
            RX_PAYLOAD6: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_PAYLOAD7 : RX_PAYLOAD6;
            RX_PAYLOAD7: uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_END      : RX_PAYLOAD7;
            RX_END:      uart_rx_nextstate = rx_ctr == UART_BIT_PERIOD ? RX_INIT     : RX_END;
            default: uart_rx_nextstate = RX_INIT;
        endcase

    always_ff @(posedge clk or negedge sifive_rxen)
        if (!sifive_rxen)
            rx_ctr <= 0;
        else if (uart_rx_nextstate != uart_rx_state || uart_rx_state == RX_INIT && uart_rx_q)
            rx_ctr <= 0;
        else
            rx_ctr <= rx_ctr + 1'b1;
            
   
    reg[7:0] rcv_frame;
    always_ff @(posedge clk or negedge sifive_rxen)
        if (!sifive_rxen)
            rcv_frame <= '0;
        else if (uart_rx_nextstate != uart_rx_state)
            case (uart_rx_state)
                RX_PAYLOAD0: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD1: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD2: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD3: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD4: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD5: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD6: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
                RX_PAYLOAD7: rcv_frame <= {uart_rx_q, rcv_frame[7:1]};
            endcase

    always_ff @(posedge clk or negedge sifive_rxen)
        if (!sifive_rxen)
            uart_rx_state <= RX_INIT;
        else
            uart_rx_state <= uart_rx_nextstate;
    
    wire rx_fifo_rvalid;
    sync_fifo #(
        .WIDTH(8)
    ) phy_rx_fifo(
		.wdata(rcv_frame), //input [7:0] Data
		.clk(clk), //input Clk
		.wvalid(uart_rx_state == RX_END && uart_rx_nextstate == RX_INIT), //input WrEn
		.rready(req && gnt && !wren && {addr[11:2], 2'b00} == 32'h4), //input RdEn
		.rstn(sifive_rxen), //input Reset
		.rdata(uart_rx_byte), //output [7:0] Q
		.rvalid(rx_fifo_rvalid), //output Empty
		.wready() //output Full
	);
	assign uart_rx_empty = ~rx_fifo_rvalid;
endmodule
