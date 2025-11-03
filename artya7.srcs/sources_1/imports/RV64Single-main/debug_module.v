module debug_module(
    input wire clk,
    input wire rstn,
    input wire[6:0] dbg_in_addr,
    input wire dbg_in_req,
    input wire dbg_in_wren,
    input wire[31:0] dbg_in_wdata,
    output wire dbg_in_ready,

    output reg dbg_out_valid,
    output reg[31:0] dbg_out,

    input wire       hart_data_req,
    input wire[31:0] hart_data_addr,
    input wire       hart_data_wren,
    input wire[7:0]  hart_data_wstrb,
    input wire[63:0] hart_data_wdata,
    output wire      hart_data_gnt,
    output reg       hart_data_rvalid,
    output reg[63:0] hart_data_rdata,

    input wire       hart_inst_req,
    input wire[31:0] hart_inst_addr,
    output wire      hart_inst_gnt,
    output reg       hart_inst_rvalid,
    output reg[31:0] hart_inst_rdata,
    output wire halt_req
    
);
    

    localparam HALTED_BASE       = 12'h100;
    localparam GOING_BASE        = HALTED_BASE + 8;
    localparam ACK_BASE          = GOING_BASE + 8;
    localparam RESUME_BASE       = ACK_BASE + 8;
    localparam ABSRACT_DATA_BASE = 12'h380;
    localparam ABSTRACT_INST_BASE = 12'h300;
    typedef enum logic[6:0] {
        DATA0 = 7'h04,
        DATA1 = 7'h05,
        DATA2 = 7'h06,
        DATA3 = 7'h07,
        DMCONTROL = 7'h10,
        DMSTATUS = 7'h11,
        ABSTRACTCS = 7'h16,
        COMMAND = 7'h17

    } debug_reg_t;

    wire[4:0] ABSTRACTCS_progbufsize = 4'd0;
    reg ABSTRACTCS_busy;
    reg[2:0] ABSTRACTCS_cmderr;
    wire[3:0] ABSTRACTCS_datacount = 4'd4;
    wire[31:0] ABSTRACTCS_REG = {3'd0, ABSTRACTCS_progbufsize, 11'd0, ABSTRACTCS_busy, 1'b0, ABSTRACTCS_cmderr, 4'd0, ABSTRACTCS_datacount};
    
    reg[2:0] ABSTRACTCS_cmderr_nxt;
    reg DMCONTROL_haltreq;
    reg DMCONTROL_resumereq;
    wire DMCONTROL_hartreset = 1'b0;
//    wire DMCONTROL_ackhavereset = 1'b0;
    wire DMCONTROL_hasel = 1'b0;
    wire[9:0] DMCONTROL_hartsello = '0;
    wire[9:0] DMCONTROL_hartselhi = '0;
    reg DMCONTROL_dmactive;
    
    wire[31:0] DMCONTROL_REG = {
        DMCONTROL_haltreq, 1'b0, DMCONTROL_hartreset, 1'b0, 1'b0,
        DMCONTROL_hasel, DMCONTROL_hartsello, DMCONTROL_hartselhi, 2'b00,
        1'b0, 1'b0, 1'b0,
        DMCONTROL_dmactive
    };
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            DMCONTROL_haltreq <= 1'b0;
            DMCONTROL_dmactive <= 1'b0;
        end
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == DMCONTROL) begin
            DMCONTROL_haltreq <= dbg_in_wdata[31];
            DMCONTROL_dmactive <= dbg_in_wdata[0];
        end

    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            DMCONTROL_resumereq <= 1'b0;
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == DMCONTROL && dbg_in_wdata[30]) begin
            DMCONTROL_resumereq <= 1'b1;
        end else if (hart_data_req && hart_data_gnt && hart_data_wren && hart_data_addr == ACK_BASE)
            DMCONTROL_resumereq <= 1'b0;

    reg[31:0] abstract_inst[8];
    reg[31:0] abstract_data[4];


    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            abstract_inst <= '{default: 32'h70000067};
        end else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == COMMAND && !ABSTRACTCS_cmderr_nxt) begin
            abstract_inst <= '{default: 32'h70000067};
            case (dbg_in_wdata[31:24])
                8'd0: if (dbg_in_wdata[17]) begin // transfer on, access register
                    if (dbg_in_wdata[15:0] < 16'h1000) begin
                        // access csr
                        abstract_inst[0] <= 32'h7b241073; // csrw dscratch0, s0
                        if (dbg_in_wdata[16]) begin
                            abstract_inst[1] <= dbg_in_wdata[22:20] == 3 ? 32'h38003403 : 32'h38006403; // ld|lwu s0, DATA0(zero)
                            abstract_inst[2] <= 32'h00041073 | {dbg_in_wdata[11:0], 20'd0}; // csrw <csr>, s0
                        end else begin
                            abstract_inst[1] <= 32'h00006473 | {dbg_in_wdata[11:0], 20'd0}; // csrr s0, <csr>
                            abstract_inst[2] <= dbg_in_wdata[22:20] == 3 ? 32'h38803023 : 32'h38802023;  // sd|sw s0, DATA0(zero)
                        end
                        abstract_inst[3] <= 32'h7b206473; // csrr s0, dscratch0
                    end else if (dbg_in_wdata[15:0] <= 16'h101F) begin
                        // access gpr
                        if (dbg_in_wdata[16]) begin
                            abstract_inst[0] <= (dbg_in_wdata[22:20] == 3 ? 32'h38003003 : 32'h38006003) | {dbg_in_wdata[4:0], 7'd0};     // ld|lwu gpr, DATA0(zero)
                        end else begin
                            abstract_inst[0] <= (dbg_in_wdata[22:20] == 3 ? 32'h38003023 : 32'h38002023) | {dbg_in_wdata[4:0], 20'd0};    // sd|sw gpr, DATA0(zero)
                        end
                    end
                end
                8'd2: if (dbg_in_wdata[16]) begin // write access
                    abstract_inst[0] <= 32'h7b241073; // csrw dscratch0, s0
                    abstract_inst[1] <= 32'h38803403; // ld s0, DATA2(zero)
                    abstract_inst[2] <= 32'h7b349073; // csrw dscratch1, s1
                    abstract_inst[3] <= 32'h38003483; // ld s1, DATA0(zero)
                    abstract_inst[4] <= dbg_in_wdata[22:20] == 3 ? 32'h00943023 : // sd s1, 0(s0) 
                                        dbg_in_wdata[22:20] == 2 ? 32'h00942023 : // sw s1, 0(s0)
                                        dbg_in_wdata[22:20] == 1 ? 32'h00941023 : // sh s1, 0(s0)
                                                                   32'h00940023;  // sb s1, 0(s0)
                    abstract_inst[5] <= 32'h7b206473; // csrr s0, dscratch0
                    abstract_inst[6] <= 32'h7b3064f3; // csrr s1, dscratch1
                end else begin
                    abstract_inst[0] <= 32'h7b241073; // csrw dscratch0, s0
                    abstract_inst[1] <= 32'h38803403; // ld s0, DATA2(zero)
                    abstract_inst[2] <= dbg_in_wdata[22:20] == 3 ? 32'h00043403 : // ld s0, 0(s0)
                                        dbg_in_wdata[22:20] == 2 ? 32'h00046403 : // lwu s0, 0(s0)
                                        dbg_in_wdata[22:20] == 1 ? 32'h00045403 : // lhu s0, 0(s0)
                                                                   32'h00044403;  // lbu s0, 0(s0)
                    abstract_inst[3] <= 32'h38803023; // sd s0, DATA0(zero)
                    abstract_inst[4] <= 32'h7b206473; // csrr s0, dscratch0
                end
            endcase
        end
    
    wire[3:0] DMSTATUS_version = 4'd2;
    wire DMSTATUS_confstrptrvalid = 1'b0;
    wire DMSTATUS_hasresethaltreq = 1'b0;
    wire DMSTATUS_authbusy = 1'b0;
    wire DMSTATUS_authenticated = 1'b1;
    reg DMSTATUS_anyhalted;
    wire DMSTATUS_allhalted = DMSTATUS_anyhalted;
    reg DMSTATUS_anyrunning;
    wire DMSTATUS_allrunning = DMSTATUS_anyrunning;
    wire DMSTATUS_anyunavail = 1'b0;
    wire DMSTATUS_allunavail = 1'b0;
    wire DMSTATUS_anynonexistent = 1'b0;
    wire DMSTATUS_allnonexistent = 1'b0;
    reg DMSTATUS_anyresumeack;
    wire DMSTATUS_allresumeack = DMSTATUS_anyresumeack;
    wire DMSTATUS_anyhavereset = 1'b0;
    wire DMSTATUS_allhavereset = 1'b0;
    wire DMSTATUS_impebreak = 1'b0;

    wire[31:0] DMSTATUS_REG = {9'd0, DMSTATUS_impebreak, 2'd0, DMSTATUS_allhavereset, DMSTATUS_anyhavereset,
                               DMSTATUS_allresumeack, DMSTATUS_anyresumeack, DMSTATUS_allnonexistent, DMSTATUS_anynonexistent, DMSTATUS_allunavail,
                               DMSTATUS_anyunavail, DMSTATUS_allrunning, DMSTATUS_anyrunning, DMSTATUS_allhalted, DMSTATUS_anyhalted,
                               DMSTATUS_authenticated, DMSTATUS_authbusy, DMSTATUS_hasresethaltreq, DMSTATUS_confstrptrvalid, DMSTATUS_version};
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            dbg_out_valid <= 1'b0;
        else
            dbg_out_valid <= dbg_in_req & dbg_in_ready;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            dbg_out <= '0;
        else if (dbg_in_req & dbg_in_ready)
            case (dbg_in_addr)
                ABSTRACTCS: dbg_out <= ABSTRACTCS_REG;
                DATA0: dbg_out <= abstract_data[0];
                DATA1: dbg_out <= abstract_data[1];
                DATA2: dbg_out <= abstract_data[2];
                DATA3: dbg_out <= abstract_data[3];
                DMCONTROL: dbg_out <= DMCONTROL_REG;
                DMSTATUS: dbg_out <= DMSTATUS_REG;
                default: dbg_out <= '0;
            endcase
    
    assign dbg_in_ready = 1'b1;

    assign hart_data_gnt = 1'b1;
    assign hart_inst_gnt = 1'b1;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            DMSTATUS_anyhalted <= 1'b0;
            DMSTATUS_anyrunning <= 1'b1;
        end
        else if (hart_data_req & hart_data_gnt & hart_data_wren)
            case (hart_data_addr)
                HALTED_BASE: begin
                    DMSTATUS_anyhalted <= 1'b1;
                    DMSTATUS_anyrunning <= 1'b0;
                end
                ACK_BASE: DMSTATUS_anyhalted <= 1'b0;
                RESUME_BASE: DMSTATUS_anyrunning <= 1'b1;
            endcase
    always_comb begin
        ABSTRACTCS_cmderr_nxt = ABSTRACTCS_cmderr;
        if (dbg_in_req & dbg_in_ready) begin
            if (ABSTRACTCS_busy) begin
                if ((dbg_in_addr == COMMAND || dbg_in_addr == ABSTRACTCS) && dbg_in_wren || // write to abstract command or status during busy is invalid
                    (dbg_in_addr == DATA0 || dbg_in_addr == DATA1 || dbg_in_addr == DATA2 || dbg_in_addr == DATA3))
                        ABSTRACTCS_cmderr_nxt = ABSTRACTCS_cmderr ? ABSTRACTCS_cmderr : 1; // CMDERR_BUSY
            end else if (!ABSTRACTCS_cmderr && dbg_in_addr == COMMAND && dbg_in_wren) begin
                case (dbg_in_wdata[31:24])
                    8'd0: begin
                        if (dbg_in_wdata[23:20] != 3)      ABSTRACTCS_cmderr_nxt = 2; // only support 32-bit and 64-bit aarsize
                        if (dbg_in_wdata[19])              ABSTRACTCS_cmderr_nxt = 2; // does not support aarpostincrement
                        if (dbg_in_wdata[18])              ABSTRACTCS_cmderr_nxt = 2; // does not support postexec
                        if (dbg_in_wdata[15:0] > 16'h101F) ABSTRACTCS_cmderr_nxt = 2; // only support CSR and GPR
                        if (dbg_in_wdata[15:0] >= 16'h7a0 && dbg_in_wdata[15:0] <= 16'h7a8) ABSTRACTCS_cmderr_nxt = 3;
                    end
                    
                    8'd2: begin
                        if (dbg_in_wdata[23])              ABSTRACTCS_cmderr_nxt = 2; // does not support virtual memory
                        if (dbg_in_wdata[22:20] > 3)       ABSTRACTCS_cmderr_nxt = 2; // only support 8-bit, 16-bit, 32-bit and 64-bit
                        if (dbg_in_wdata[19])              ABSTRACTCS_cmderr_nxt = 2; // not support aam
                        if (dbg_in_wdata[18:17])           ABSTRACTCS_cmderr_nxt = 2; // reserved, must be 0
                        if (dbg_in_wdata[15:0])            ABSTRACTCS_cmderr_nxt = 2; // reserved, must be 0
                        
                    end
                    default: ABSTRACTCS_cmderr_nxt = 2;
                endcase
                if (~DMSTATUS_anyhalted) ABSTRACTCS_cmderr_nxt = 4;
            end else if (dbg_in_addr == ABSTRACTCS && dbg_in_wren)
                ABSTRACTCS_cmderr_nxt = ABSTRACTCS_cmderr & ~dbg_in_wdata[10:8];
        end
    end

    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            ABSTRACTCS_busy <= 1'b0;
        else if (hart_data_req && hart_data_gnt && hart_data_wren && hart_data_addr == HALTED_BASE)
            ABSTRACTCS_busy <= 1'b0;
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == COMMAND && !ABSTRACTCS_cmderr_nxt)
            ABSTRACTCS_busy <= 1'b1;

    reg going_flag;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            going_flag <= 1'b0;
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == COMMAND && !ABSTRACTCS_cmderr_nxt)
            going_flag <= 1'b1;
        else if (hart_data_req && hart_data_gnt && hart_data_wren && hart_data_addr == GOING_BASE)
            going_flag <= 1'b0;
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            ABSTRACTCS_cmderr <= '0;
        else
            ABSTRACTCS_cmderr <= ABSTRACTCS_cmderr_nxt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            hart_data_rvalid <= 1'b0;
        else
            hart_data_rvalid <= hart_data_req & hart_data_gnt;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            hart_data_rdata <= '0;
        else if (hart_data_req & hart_data_gnt)
            case (hart_data_addr)
                ABSRACT_DATA_BASE: hart_data_rdata <= {abstract_data[1], abstract_data[0]};
                ABSRACT_DATA_BASE+4: hart_data_rdata <= {abstract_data[1], abstract_data[1]};
                ABSRACT_DATA_BASE+8: hart_data_rdata <= {abstract_data[3], abstract_data[2]};
                ABSRACT_DATA_BASE+12: hart_data_rdata <= {abstract_data[3], abstract_data[3]};
                GOING_BASE: hart_data_rdata <= going_flag;
                ACK_BASE: hart_data_rdata <= DMCONTROL_resumereq;
                default: hart_data_rdata <= '0;
            endcase
        

    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            abstract_data <= '{default: '0};
        end else if (hart_data_req && hart_data_gnt && hart_data_wren)
            case (hart_data_addr)
                ABSRACT_DATA_BASE: begin
                    if (hart_data_wstrb[0])
                        abstract_data[0] <= hart_data_wdata[0+:32];
                    if (hart_data_wstrb[4])
                        abstract_data[1] <= hart_data_wdata[32+:32]; 
                end
                ABSRACT_DATA_BASE+4: if (hart_data_wstrb[0]) abstract_data[1] <= hart_data_wdata[0+:32]; 
                ABSRACT_DATA_BASE+8: begin
                    if (hart_data_wstrb[0])
                        abstract_data[2] <= hart_data_wdata[0+:32];
                    if (hart_data_wstrb[4])
                        abstract_data[3] <= hart_data_wdata[32+:32]; 
                end
                ABSRACT_DATA_BASE+12: if (hart_data_wstrb[0]) abstract_data[3] <= hart_data_wdata[0+:32]; 
            endcase
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && !ABSTRACTCS_busy)
            case (dbg_in_addr)
                DATA0: abstract_data[0] <= dbg_in_wdata;
                DATA1: abstract_data[1] <= dbg_in_wdata;
                DATA2: abstract_data[2] <= dbg_in_wdata;
                DATA3: abstract_data[3] <= dbg_in_wdata;
            endcase
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            DMSTATUS_anyresumeack <= 1'b0;
        else if (hart_data_req && hart_data_gnt && hart_data_wren && hart_data_addr == ACK_BASE)
            DMSTATUS_anyresumeack <= 1'b1;
        else if (dbg_in_req && dbg_in_ready && dbg_in_wren && dbg_in_addr == DMCONTROL && dbg_in_wdata[30])
            DMSTATUS_anyresumeack <= 1'b0;

    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            hart_inst_rvalid <= 1'b0;
            hart_inst_rdata <= '0;
        end else if (hart_inst_req & hart_inst_gnt) begin
            hart_inst_rvalid <= 1'b1;
            case (hart_inst_addr)
                ABSTRACT_INST_BASE: hart_inst_rdata <= abstract_inst[0];
                ABSTRACT_INST_BASE+4: hart_inst_rdata <= abstract_inst[1];
                ABSTRACT_INST_BASE+8: hart_inst_rdata <= abstract_inst[2];
                ABSTRACT_INST_BASE+12: hart_inst_rdata <= abstract_inst[3];
                ABSTRACT_INST_BASE+16: hart_inst_rdata <= abstract_inst[4];
                ABSTRACT_INST_BASE+20: hart_inst_rdata <= abstract_inst[5];
                ABSTRACT_INST_BASE+24: hart_inst_rdata <= abstract_inst[6];
                ABSTRACT_INST_BASE+28: hart_inst_rdata <= abstract_inst[7];
            endcase
        end else
            hart_inst_rvalid <= 1'b0;
    assign halt_req = DMCONTROL_haltreq;
endmodule