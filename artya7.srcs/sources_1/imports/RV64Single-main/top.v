`default_nettype none


module sd_ctrl_bridge #(
    parameter CACHE_LINE = 0,
    parameter SECTOR_BUS_WIDTH = 0
    
)(
    input wire clk,
    input wire rstn,
    output wire[5:0] debug_out,
    input wire bus_req,
    input wire bus_wren,
    input wire[31:0] bus_addr,
    output wire bus_gnt,
    input wire bus_wvalid,
    input wire[SECTOR_BUS_WIDTH-1:0] bus_wdata,
    output wire bus_wready,
    output wire bus_rvalid,
    output wire[SECTOR_BUS_WIDTH-1:0] bus_rdata,
    
    output wire sram_sck,
    output wire sram_cs,
    inout wire sram_hold,
    inout wire sram_mosi,
    inout wire sram_dq2,
    inout wire sram_miso
);
    assign debug_out = '1;
    reg[CACHE_LINE*8-1:0] in_wdata;
    reg[31:0] in_addr;
    wire in_ready;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            in_addr <= '0;
        else if (bus_gnt & bus_req)
            in_addr <= bus_addr;
    typedef enum {
        SD_IDLE,
        SD_READ_REQ,
        SD_WRITE_BUFFERING,
        SD_WRITE_REQ
    } SD_STATE;
    SD_STATE state;
    localparam WCOUNTER_LIMIT = 8*CACHE_LINE/SECTOR_BUS_WIDTH-1;
    reg[15:0] state_ctr;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            state <= SD_IDLE;
            state_ctr <= '0;
            in_wdata <= '0;
        end
        else
            case (state)
                SD_IDLE: if (bus_req) begin
                    state <= bus_wren ? SD_WRITE_BUFFERING : SD_READ_REQ;
                    state_ctr <= '0;
                end
                SD_READ_REQ: if (in_ready) state <= SD_IDLE;
                SD_WRITE_BUFFERING: if (bus_wvalid) begin
                    in_wdata <= {bus_wdata, in_wdata[8*CACHE_LINE-1:SECTOR_BUS_WIDTH]};
                    state_ctr <= state_ctr + 1'b1;
                    if (state_ctr == WCOUNTER_LIMIT) begin
                        state <= SD_WRITE_REQ;
                    end
                end
                SD_WRITE_REQ: if (in_ready) state <= SD_IDLE;
            endcase
    assign bus_wready = state == SD_WRITE_BUFFERING;
    assign bus_gnt = state == SD_IDLE;
    wire in_wren = state == SD_WRITE_REQ;
    wire in_valid = state == SD_WRITE_REQ || state == SD_READ_REQ;
    sram_controller #(
        .SECTOR_BUS_WIDTH(SECTOR_BUS_WIDTH),
        .BURST_BYTES(CACHE_LINE)
    ) sram0(
        .clk,
        .rstn,

        .sram_sck,
        .sram_cs,
        .sram_hold,
        .sram_mosi,
        .sram_dq2,
        .sram_miso,
        
        .in_addr,
        .in_wren,
        .in_wdata,
        .in_valid,
        .in_ready,

        .word_out_valid(bus_rvalid),
        .word_out(bus_rdata)
    );


    
endmodule

module top(
	input wire in_rst,
    input wire clk,

    input wire uart0_rx,
    output wire uart0_tx,

    

    input wire jtag_tck,
    input wire jtag_tdi,
    input wire jtag_tms,
    output wire jtag_tdo,

    input wire spi0_miso,
    output wire spi0_clk,
    output wire spi0_cs,
    output wire spi0_mosi,

    input wire spi1_miso,
    output wire spi1_clk,
    output wire spi1_cs,
    output wire spi1_mosi,
    inout wire[1:0] gpio
    
);

    

    wire uart1_rx;
    wire uart1_tx;
	wire sram_sck;
    wire sram_cs;
    wire sram_hold;
    wire sram_mosi;
    wire sram_dq2;
    wire sram_miso;
    wire in_rstn = ~in_rst;
    reg[3:0] sync_rstn = '0;
    always_ff @(posedge clk or negedge in_rstn)
    	if (!in_rstn)
		sync_rstn <= '0;
	else
		sync_rstn <= {1'b1, sync_rstn[3:1]};
	reg rstn;
    always_ff @(posedge clk or negedge sync_rstn[0])
    	if (!sync_rstn[0])
		rstn <= 1'b0;
	else
		rstn <= 1'b1;
    localparam MEM_BASE = 32'h8000_0000;
    localparam ABSTRACT_INST_BASE = 12'h300;
    localparam ABSRACT_DATA_BASE = 12'h380;
    parameter CLINT0_BASE = 32'h2000000;
    parameter CLINT0_SIZE = 32'h10000;
    localparam DB_ROM_BASE = 32'h700;
    localparam DB_HALTED_BASE = 32'h100;
    localparam GPIO_BASE  = 32'h10012000;
    localparam GPIO_SIZE  = 32'h1000;
    localparam UART0_BASE = 32'h10013000;
    localparam UART0_SIZE = 32'h00001000;
    localparam UART1_BASE = 32'h10023000;
    localparam UART1_SIZE = 32'h00001000;
    localparam SPI0_BASE  = 32'h10024000;
    localparam SPI0_SIZE  = 32'h1000;
    localparam SPI1_BASE  = 32'h10034000;
    localparam SPI1_SIZE  = 32'h1000;
    localparam BOOTROM_BASE = 32'h10000;
    localparam BOOTROM_SIZE = 32'h10000;
    localparam SCRATCH_BASE = 32'h7000_0000;
    localparam SCRATCH_SIZE = 1<<14;
    
   
    
    wire dmem_req;
    wire[31:0] dmem_addr;
    wire dmem_wren;
    wire[7:0] dmem_wstrobe;
    wire[63:0] dmem_wdata;
    wire dmem_gnt;
    wire dmem_rvalid;
    wire[63:0] dmem_rdata;
    
    localparam SECTOR_BUS_WIDTH = 8;
    localparam CACHE_LINE = 32;
    wire bus_req;
    wire bus_wren;
    wire[31:0] bus_addr;
    wire bus_gnt;
    wire bus_wvalid;
    wire[SECTOR_BUS_WIDTH-1:0] bus_wdata;
    wire bus_wready;
    wire bus_rvalid;
    wire[SECTOR_BUS_WIDTH-1:0] bus_rdata;

    wire imem_req;
    wire[31:0] imem_addr;
    wire imem_rvalid;
    wire[31:0] imem_rdata;
    wire imem_gnt;
    
    wire icache_sec_req;
    wire[31:0] icache_sec_addr;
    wire dcache_sec_req;
    wire[31:0] dcache_sec_addr;
    wire dmem_sec_wren;

    assign bus_addr = icache_sec_req ? icache_sec_addr : dcache_sec_addr;
    assign bus_req = icache_sec_req | dcache_sec_req;
    assign bus_wren = dmem_sec_wren & !icache_sec_req;

    wire clint0_MTIP0;
    wire clint0_MSIP0;
    RISCV_CLINT clint0(
            .clk,
            .rstn,
            .addr(slave_dfetch_if[1].addr - CLINT0_BASE),
            .req(slave_dfetch_if[1].avalid),
            .wren(slave_dfetch_if[1].awren),
            .gnt(slave_dfetch_if[1].aready),
            .wstrb(slave_dfetch_if[1].awstrb),
            .wdata(slave_dfetch_if[1].awdata),
            .rdata(slave_dfetch_if[1].rdata),
            .rvalid(slave_dfetch_if[1].rvalid),
            .out_mtime(),
            .out_MTIP0(clint0_MTIP0),
            .out_MSIP0(clint0_MSIP0)
    );

    wire dmi_rstn;
    wire dmi_req_valid;
    wire dmi_req_ready;
    wire[6:0] dmi_req_addr;
    wire dmi_req_wren;
    wire[31:0] dmi_req_data;
    wire dmi_resp_valid;
    wire[31:0] dmi_resp_data;
    
    dtm_verilog #(
        .IDCODE_VERSION(0),
        .IDCODE_PARTID(0),
        .IDCODE_MANID(0)
    ) dtm0(
        .clk_i(clk),
        .rstn_i(rstn),
        .jtag_trst_i(1'b1),
        .jtag_tck_i(jtag_tck),
        .jtag_tdi_i(jtag_tdi),
        .jtag_tdo_o(jtag_tdo),
        .jtag_tms_i(jtag_tms),

        .dmi_rstn_o(dmi_rstn),
        .dmi_req_valid_o(dmi_req_valid),
        .dmi_req_ready_i(dmi_req_ready),
        .dmi_req_addr_o(dmi_req_addr),
        .dmi_req_op_o(dmi_req_wren),
        .dmi_resp_err_i(1'b0),
        .dmi_req_data_o(dmi_req_data),
        .dmi_resp_valid_i(dmi_resp_valid),
        .dmi_resp_data_i(dmi_resp_data)
    );

    Icache #(.SECTOR_BUS_WIDTH(SECTOR_BUS_WIDTH), .CACHE_LINE(CACHE_LINE)) icache0(
        .clk(clk),
        .rstn(rstn),
        .mem_req(slave_ifetch_if[0].arvalid),
        .mem_gnt(slave_ifetch_if[0].arready),
        .mem_addr_w(slave_ifetch_if[0].araddr - MEM_BASE),
        .mem_rvalid(slave_ifetch_if[0].rvalid),
        .mem_rdata(slave_ifetch_if[0].rdata),
        
        .sector_req(icache_sec_req),
        .sector_addr(icache_sec_addr),
        .sector_gnt(bus_gnt),
        .sector_rvalid(bus_rvalid),
        .sector_rdata(bus_rdata),
        .icache_flush(cpu_ifetch_if.arvalid && cpu_ifetch_if.arready && cpu_ifetch_if.araddr < 32'h8000_0000)
        
    );

    boot_rom bootrom0(
            .clk,
            .rstn,
            .addr(slave_ifetch_if[3].araddr-BOOTROM_BASE),
            .addr_valid(slave_ifetch_if[3].arvalid),
            .addr_ready(slave_ifetch_if[3].arready),
            .rvalid(slave_ifetch_if[3].rvalid),
            .rdata(slave_ifetch_if[3].rdata),

        .addr2_valid(slave_dfetch_if[8].avalid),
        .addr2(slave_dfetch_if[8].addr-BOOTROM_BASE),
        .addr2_ready(slave_dfetch_if[8].aready),
        .rvalid2(slave_dfetch_if[8].rvalid),
        .rdata2(slave_dfetch_if[8].rdata)
    );
    debug_rom rom0(
            .clk,
            .rstn,
            .addr(slave_ifetch_if[2].araddr-DB_ROM_BASE),
            .addr_valid(slave_ifetch_if[2].arvalid),
            .addr_ready(slave_ifetch_if[2].arready),
            .rvalid(slave_ifetch_if[2].rvalid),
            .rdata(slave_ifetch_if[2].rdata)
    );
    wire halt_req;
    debug_module dm0(
        .clk(clk),
        .rstn(dmi_rstn),
        .dbg_in_addr(dmi_req_addr),
        .dbg_in_req(dmi_req_valid),
        .dbg_in_wren(dmi_req_wren),
        .dbg_in_wdata(dmi_req_data),
        .dbg_in_ready(dmi_req_ready),
        .dbg_out_valid(dmi_resp_valid),
        .dbg_out(dmi_resp_data),
        
        .hart_data_req(slave_dfetch_if[3].avalid),
        .hart_data_addr(slave_dfetch_if[3].addr),
        .hart_data_wren(slave_dfetch_if[3].awren),
        .hart_data_wstrb(slave_dfetch_if[3].awstrb),
        .hart_data_wdata(slave_dfetch_if[3].awdata),
        .hart_data_gnt(slave_dfetch_if[3].aready),
        .hart_data_rvalid(slave_dfetch_if[3].rvalid),
        .hart_data_rdata(slave_dfetch_if[3].rdata),
        
        .hart_inst_req(slave_ifetch_if[1].arvalid),
        .hart_inst_addr(slave_ifetch_if[1].araddr),
        .hart_inst_gnt(slave_ifetch_if[1].arready),
        .hart_inst_rvalid(slave_ifetch_if[1].rvalid),
        .hart_inst_rdata(slave_ifetch_if[1].rdata),
        .halt_req(halt_req)
        
    );
    
    dcache128 #(.SECTOR_BUS_WIDTH(SECTOR_BUS_WIDTH), .CACHE_LINE(CACHE_LINE)) cache0(
        .clk(clk),
        .rstn(rstn),
        .mem_req(slave_dfetch_if[0].avalid),
        .mem_gnt(slave_dfetch_if[0].aready),
        .mem_addr(slave_dfetch_if[0].addr - MEM_BASE),
        .mem_wdata(slave_dfetch_if[0].awdata),
        .mem_wstrobe(slave_dfetch_if[0].awstrb),
        .mem_rvalid(slave_dfetch_if[0].rvalid),
        .mem_rdata(slave_dfetch_if[0].rdata),
        .mem_wren(slave_dfetch_if[0].awren),
        
        .sector_req(dcache_sec_req),
        .sector_wren(dmem_sec_wren),
        .sector_addr(dcache_sec_addr),
        .sector_gnt(bus_gnt & !icache_sec_req),
        .sector_wvalid(bus_wvalid),
        .sector_wdata(bus_wdata),
        .sector_wready(bus_wready),
        .sector_rvalid(bus_rvalid),
        .sector_rdata(bus_rdata)
        
    );

    sd_ctrl_bridge #(.SECTOR_BUS_WIDTH(SECTOR_BUS_WIDTH), .CACHE_LINE(CACHE_LINE)) sd0(
        .clk(clk), .rstn(rstn), 

        .bus_req,
        .bus_wren,
        .bus_addr,
        .bus_gnt,

        .bus_wvalid,
        .bus_wdata,
        .bus_wready,

        .bus_rvalid,
        .bus_rdata,
.sram_sck,
        .sram_cs,
        .sram_hold,
        .sram_mosi,
        .sram_dq2,
        .sram_miso
    );

    localparam IMEM_N_SLAVES = 5;
    localparam[31:0] IMEM_ADDRESS_RANGES[IMEM_N_SLAVES][2] = '{
        {32'(MEM_BASE), 32'h9000_0000},
        {32'(ABSTRACT_INST_BASE), 32'(ABSTRACT_INST_BASE+32)},
        {32'(DB_ROM_BASE), 32'(DB_ROM_BASE+4*32)},
        {32'(BOOTROM_BASE), 32'(BOOTROM_BASE + BOOTROM_SIZE)},
        
        {32'(SCRATCH_BASE), 32'(SCRATCH_BASE + SCRATCH_SIZE)}
    };
    slv_interface_ro cpu_ifetch_if();
    slv_interface_ro slave_ifetch_if[IMEM_N_SLAVES](); 

    assign cpu_ifetch_if.arvalid = imem_req;
    assign cpu_ifetch_if.araddr = imem_addr;
    assign cpu_ifetch_if.rready = 1'b1; 
    assign imem_gnt = cpu_ifetch_if.arready;
    assign imem_rdata = cpu_ifetch_if.rdata;
    assign imem_rvalid = cpu_ifetch_if.rvalid;


    
    single_master_multi_slave_ro #(
        .N_SLAVES(IMEM_N_SLAVES),
        .ADDRESS_RANGES(IMEM_ADDRESS_RANGES)
    ) ifetch_bus(
        clk,
        rstn,
        cpu_ifetch_if,
        slave_ifetch_if
    );
    localparam DMEM_N_SLAVES = 10;
    localparam[31:0] DMEM_ADDRESS_RANGES[DMEM_N_SLAVES][2] = '{
        {32'(MEM_BASE), 32'h9000_0000},
        {32'(CLINT0_BASE), 32'(CLINT0_BASE+CLINT0_SIZE)},
        {32'(UART0_BASE), 32'(UART0_BASE + UART0_SIZE)},
        {32'h100, 32'h400}, // abstract data, halted
        {32'(GPIO_BASE), 32'(GPIO_BASE + GPIO_SIZE)},
        {32'(SPI0_BASE), 32'(SPI0_BASE + SPI0_SIZE)},
        {32'(UART1_BASE), 32'(UART1_BASE + UART1_SIZE)},
        {32'(SPI1_BASE), 32'(SPI1_BASE + SPI1_SIZE)},
        {32'(BOOTROM_BASE), 32'(BOOTROM_BASE + BOOTROM_SIZE)},
        
        {32'(SCRATCH_BASE), 32'(SCRATCH_BASE + SCRATCH_SIZE)}
    }; 
    slv_interface #(.WIDTH(64)) cpu_dfetch_if();
    slv_interface #(.WIDTH(64)) slave_dfetch_if[DMEM_N_SLAVES](); 

    assign cpu_dfetch_if.avalid = dmem_req;
    assign cpu_dfetch_if.addr = dmem_addr;
    assign cpu_dfetch_if.awstrb = dmem_wstrobe;
    assign cpu_dfetch_if.awren = dmem_wren;
    assign cpu_dfetch_if.awdata = dmem_wdata;
    assign cpu_dfetch_if.rready = 1'b1; 
    assign dmem_gnt = cpu_dfetch_if.aready;
    assign dmem_rdata = cpu_dfetch_if.rdata;
    assign dmem_rvalid = cpu_dfetch_if.rvalid;

    single_master_multi_slave #(
        .N_SLAVES(DMEM_N_SLAVES),
        .ADDRESS_RANGES(DMEM_ADDRESS_RANGES)
    ) dfetch_bus(
        clk,
        rstn,
        cpu_dfetch_if,
        slave_dfetch_if
    );

    Scratch4096 srcatch0(
        .clk,
        .rstn,

    .addr(slave_dfetch_if[9].addr - SCRATCH_BASE),
            .req(slave_dfetch_if[9].avalid),
            .wren(slave_dfetch_if[9].awren),
            .gnt(slave_dfetch_if[9].aready),
            .wstrb(slave_dfetch_if[9].awstrb),
            .wdata(slave_dfetch_if[9].awdata),
            .rdata(slave_dfetch_if[9].rdata),
            .rvalid(slave_dfetch_if[9].rvalid),

    .addr2(slave_ifetch_if[4].araddr-SCRATCH_BASE),
            .addr_valid2(slave_ifetch_if[4].arvalid),
            .addr_ready2(slave_ifetch_if[4].arready),
            .rvalid2(slave_ifetch_if[4].rvalid),
            .rdata2(slave_ifetch_if[4].rdata)
);
    SIFIVE_UART uart0(
            .clk,
            .rstn,
            .addr(slave_dfetch_if[2].addr - UART0_BASE),
            .req(slave_dfetch_if[2].avalid),
            .wren(slave_dfetch_if[2].awren),
            .gnt(slave_dfetch_if[2].aready),
            .wstrb(slave_dfetch_if[2].awstrb),
            .wdata(slave_dfetch_if[2].awdata),
            .rdata(slave_dfetch_if[2].rdata),
            .rvalid(slave_dfetch_if[2].rvalid),
            .uart_rx(uart0_rx),
            .uart_tx(uart0_tx)
    );

    SIFIVE_UART uart1(
            .clk,
            .rstn,
            .addr(slave_dfetch_if[6].addr - UART1_BASE),
            .req(slave_dfetch_if[6].avalid),
            .wren(slave_dfetch_if[6].awren),
            .gnt(slave_dfetch_if[6].aready),
            .wstrb(slave_dfetch_if[6].awstrb),
            .wdata(slave_dfetch_if[6].awdata),
            .rdata(slave_dfetch_if[6].rdata),
            .rvalid(slave_dfetch_if[6].rvalid),
            .uart_rx(uart1_rx),
            .uart_tx(uart1_tx)
    );
    SIFIVE_GPIO gpio0(
            .clk,
            .rstn,
            .addr(slave_dfetch_if[4].addr - GPIO_BASE),
            .req(slave_dfetch_if[4].avalid),
            .wren(slave_dfetch_if[4].awren),
            .gnt(slave_dfetch_if[4].aready),
            .wstrb(slave_dfetch_if[4].awstrb),
            .wdata(slave_dfetch_if[4].awdata),
            .rdata(slave_dfetch_if[4].rdata),
            .rvalid(slave_dfetch_if[4].rvalid),
            .gpio_io(gpio)
    );

    SIFIVE_SPI spi0(
        .clk,
        .rstn,
        .addr(slave_dfetch_if[5].addr - SPI0_BASE),
            .req(slave_dfetch_if[5].avalid),
            .wren(slave_dfetch_if[5].awren),
            .gnt(slave_dfetch_if[5].aready),
            .wstrb(slave_dfetch_if[5].awstrb),
            .wdata(slave_dfetch_if[5].awdata),
            .rdata(slave_dfetch_if[5].rdata),
            .rvalid(slave_dfetch_if[5].rvalid),
            .spi_cs(spi0_cs),
            .spi_clk(spi0_clk),
        .spi_mosi(spi0_mosi),
        .spi_miso(spi0_miso)
    
    );

    SIFIVE_SPI spi1(
        .clk,
        .rstn,
        .addr(slave_dfetch_if[7].addr - SPI1_BASE),
            .req(slave_dfetch_if[7].avalid),
            .wren(slave_dfetch_if[7].awren),
            .gnt(slave_dfetch_if[7].aready),
            .wstrb(slave_dfetch_if[7].awstrb),
            .wdata(slave_dfetch_if[7].awdata),
            .rdata(slave_dfetch_if[7].rdata),
            .rvalid(slave_dfetch_if[7].rvalid),

            .spi_cs(spi1_cs),
            .spi_clk(spi1_clk),
        .spi_mosi(spi1_mosi),
        .spi_miso(spi1_miso)
    
    );

    riscv riscv0(
        .clk,
        .rstn,
        .dmem_req,
        .dmem_addr,
        .dmem_wren,
        .dmem_wstrobe,
        .dmem_wdata,
        .dmem_rvalid,
        .dmem_rdata,
        .dmem_gnt,
        .imem_req,
        .imem_addr,
        .imem_rvalid,
        .imem_rdata,
        .imem_gnt,
        .in_MTIP(clint0_MTIP0),
        .in_MSIP(clint0_MSIP0),
        .in_MEIP(1'b0),
        .in_HALTP(halt_req)
    );
endmodule
