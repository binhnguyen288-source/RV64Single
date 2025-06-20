interface slv_interface_ro #(
    parameter WIDTH = 32,
    parameter ARWIDTH = 32
)();
    // address channel input
    wire arvalid;
    wire[ARWIDTH-1:0] araddr;

    // address channel output
    wire arready;
    
    // read channel output
    wire rvalid;
    wire[WIDTH-1:0] rdata;
    
    // read channel input
    wire rready;

    modport slv(input arvalid, input araddr, output arready, output rvalid, output rdata, input rready);
    modport master(output arvalid, output araddr, input arready, input rvalid, input rdata, output rready);
endinterface



module single_master_multi_slave_ro #(
    parameter N_SLAVES = 1,
    parameter[31:0] ADDRESS_RANGES[N_SLAVES][2] = '{default: '0}
) (
    input wire clk,
    input wire rstn,
    slv_interface_ro.slv master,
    slv_interface_ro.master slv[N_SLAVES]
);


    localparam SLV_IDX_WIDTH = $clog2(N_SLAVES);
    for (genvar i = 0; i < N_SLAVES; ++i) begin
        if (master.WIDTH != slv[i].WIDTH || master.ARWIDTH != slv[i].ARWIDTH)
            $error("slave ID WIDTH error!");
        if (ADDRESS_RANGES[i][0] >= ADDRESS_RANGES[i][1])
            $error("address base larger than end");
        for (genvar j = i + 1; j < N_SLAVES; ++j) begin
            if (ADDRESS_RANGES[i][0] == ADDRESS_RANGES[j][0])
                $error("slave address overlapped!");
        end
    end

    wire outstanding_avail;
    
    wire[N_SLAVES-1:0] slave_sel;
    wire[N_SLAVES-1:0] slave_ready;
    wire[N_SLAVES-1:0] slave_rvalid;
    wire[master.WIDTH-1:0] slave_rdata[N_SLAVES];

    reg[SLV_IDX_WIDTH-1:0] slv_idx_q;
    wire[SLV_IDX_WIDTH-1:0] slave_aridx[N_SLAVES];
    always @(posedge clk or negedge rstn)
        if (!rstn)
            slv_idx_q <= '0;
        else if (master.arready & master.arvalid)
            slv_idx_q <= slave_aridx[0];
    for (genvar i = 0; i < N_SLAVES; ++i) begin
        assign slave_sel[i] = master.araddr >= ADDRESS_RANGES[i][0] && master.araddr < ADDRESS_RANGES[i][1];        
        assign slave_ready[i] = slv[i].arready;
        assign slave_rvalid[i] = slv[i].rvalid;
        assign slave_rdata[i] = slv[i].rdata;


        assign slv[i].araddr = master.araddr;
        assign slv[i].arvalid = master.arvalid & slave_sel[i] & outstanding_avail;
        assign slv[i].rready = i == slv_idx_q ? master.rready : 1'b0;
    end
    assign slave_aridx[N_SLAVES-1] = N_SLAVES-1;
    for (genvar i = 0; i < N_SLAVES-1; ++i) begin
        assign slave_aridx[i] = slave_sel[i] ? i : slave_aridx[i+1];
    end
    reg wait_rvalid;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            wait_rvalid <= 1'b0;
        else if (outstanding_avail)
            wait_rvalid <= master.arready & master.arvalid;
    assign outstanding_avail = ~wait_rvalid || master.rready && master.rvalid;

    assign master.rvalid = slave_rvalid[slv_idx_q];
    assign master.rdata = slave_rdata[slv_idx_q];
    assign master.arready = slave_ready[slave_aridx[0]] & outstanding_avail;
    
   
endmodule


interface slv_interface #(
    parameter WIDTH = 32,
    parameter ARWIDTH = 32
)();
    // address channel input
    wire avalid;
    wire awren;
    wire[WIDTH/8-1:0] awstrb;
    wire[WIDTH-1:0] awdata;
    wire[ARWIDTH-1:0] addr;

    // address channel output
    wire aready;
    
    // read channel output
    wire rvalid;
    wire[WIDTH-1:0] rdata;
    
    // read channel input
    wire rready;
    modport slv(input avalid, input awren, input awstrb, input awdata, input addr, output aready, output rvalid, output rdata, input rready);
    modport master(output avalid, output awren, output awstrb, output awdata, output addr, input aready, input rvalid, input rdata, output rready);
endinterface




module single_master_multi_slave #(
    parameter N_SLAVES = 1,
    parameter[31:0] ADDRESS_RANGES[N_SLAVES][2] = '{default: '0}
) (
    input wire clk,
    input wire rstn,
    slv_interface.slv master,
    slv_interface.master slv[N_SLAVES]
);


    localparam SLV_IDX_WIDTH = $clog2(N_SLAVES);
    for (genvar i = 0; i < N_SLAVES; ++i) begin
        if (master.WIDTH != slv[i].WIDTH || master.ARWIDTH != slv[i].ARWIDTH)
            $error("slave ID WIDTH error!");
        if (ADDRESS_RANGES[i][0] >= ADDRESS_RANGES[i][1])
            $error("address base larger than end");
        for (genvar j = i + 1; j < N_SLAVES; ++j) begin
            if (ADDRESS_RANGES[i][0] == ADDRESS_RANGES[j][0])
                $error("slave address overlapped!");
        end
    end

    wire outstanding_avail;
    
    wire[N_SLAVES-1:0] slave_sel;
    wire[N_SLAVES-1:0] slave_ready;
    wire[N_SLAVES-1:0] slave_rvalid;
    wire[master.WIDTH-1:0] slave_rdata[N_SLAVES];

    reg[SLV_IDX_WIDTH-1:0] slv_idx_q;
    wire[SLV_IDX_WIDTH-1:0] slave_aridx[N_SLAVES];
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            slv_idx_q <= '0;
        else if (master.aready & master.avalid)
            slv_idx_q <= slave_aridx[0];
    for (genvar i = 0; i < N_SLAVES; ++i) begin
        assign slave_sel[i] = master.addr >= ADDRESS_RANGES[i][0] && master.addr < ADDRESS_RANGES[i][1];        
        assign slave_ready[i] = slv[i].aready;
        assign slave_rvalid[i] = slv[i].rvalid;
        assign slave_rdata[i] = slv[i].rdata;


        assign slv[i].addr = master.addr;
        assign slv[i].awren = master.awren;
        assign slv[i].awstrb = master.awstrb;
        assign slv[i].awdata = master.awdata;
        assign slv[i].avalid = master.avalid & slave_sel[i] & outstanding_avail;
        assign slv[i].rready = i == slv_idx_q ? master.rready : 1'b0;
    end
    assign slave_aridx[N_SLAVES-1] = N_SLAVES-1;
    for (genvar i = 0; i < N_SLAVES-1; ++i) begin
        assign slave_aridx[i] = slave_sel[i] ? i : slave_aridx[i+1];
    end
    reg wait_rvalid;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            wait_rvalid <= 1'b0;
        else if (outstanding_avail)
            wait_rvalid <= master.aready & master.avalid;
    assign outstanding_avail = ~wait_rvalid || master.rready && master.rvalid;

    assign master.rvalid = slave_rvalid[slv_idx_q];
    assign master.rdata = slave_rdata[slv_idx_q];
    assign master.aready = slave_ready[slave_aridx[0]] & outstanding_avail;
    
   
endmodule