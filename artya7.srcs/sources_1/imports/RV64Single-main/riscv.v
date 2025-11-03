`default_nettype none

module ALU_RISCV(
    input wire[31:0] instruction,
    input wire[63:0] operand_1,
    input wire[63:0] operand_2,
    output wire[63:0] result
);

    wire must_add = instruction[14:12] == 3'b000 && !(instruction[5] & instruction[30]);

    wire[64:0] alu_addsub65 = must_add ? operand_1 + operand_2 : operand_1 - operand_2;
    
    wire cmp_lt   = operand_1[63] != operand_2[63] ? operand_1[63] : alu_addsub65[64];
    wire cmp_ltu  = alu_addsub65[64];
    reg[63:0] alu_result;
//    wire[31:0] shift_result;
//    barrel_shifter32 shifter0(
//        .in(operand_1), .amt(operand_2[4:0]),
//        .sra(instruction[30]), .is_shift_right(instruction[14]), 
//        .out(shift_result)
//    );
    wire signed[63:0] sra64 = $signed($signed(operand_1) >>> operand_2[5:0]);
    wire signed[31:0] sra_32 = $signed($signed(operand_1[31:0]) >>> operand_2[4:0]);
    wire[31:0] srl_32 = operand_1[31:0] >> operand_2[4:0];
    wire[31:0] sll_32 = operand_1[31:0] << operand_2[4:0];
    always_comb
        case (instruction[14:12])
            3'b000: alu_result = instruction[3] ? {{32{alu_addsub65[31]}}, alu_addsub65[31:0]}: alu_addsub65[63:0];
            3'b001: alu_result = instruction[3] ? {{32{sll_32[31]}}, sll_32[31:0]} : operand_1 << operand_2[5:0]; // shift left
            3'b101: alu_result = instruction[30] ? 
                                 (instruction[3] ? {{32{sra_32[31]}}, sra_32[31:0]} : sra64) : 
                                 (instruction[3] ? {{32{srl_32[31]}}, srl_32[31:0]} : operand_1 >> operand_2[5:0]); // shift right
            3'b010: alu_result = cmp_lt;
            3'b011: alu_result = cmp_ltu;
            3'b100: alu_result = operand_1 ^ operand_2;
            3'b110: alu_result = operand_1 | operand_2;
            3'b111: alu_result = operand_1 & operand_2;
        endcase
    assign result = alu_result;
endmodule

module REGISTER_FILE(
    input wire clk,
    input wire rstn,
    input wire wr_en,
    input wire rd_en,
    input wire[4:0] wr_addr,
    input wire[63:0] wr_data,
    
    input wire[4:0] rd_addr1,
    output reg[63:0] rd_data1,
    input wire[4:0] rd_addr2,
    output reg[63:0] rd_data2
) ;
    
    reg[63:0] x[32];
    for (genvar i = 0; i < 32; ++i) begin
    	always_ff @(posedge clk or negedge rstn)
		if (!rstn)
			x[i] <= '0;
        	else if (wr_en && wr_addr == i)
            		x[i] <= wr_data;
    end
    always_ff @(posedge clk or negedge rstn)
    	if (!rstn)
		rd_data1 <= '0;
	else if (rd_en) begin
            if (rd_addr1 == 0)
                rd_data1 <= 0;
            else
                rd_data1 <= x[rd_addr1];
        end
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
		rd_data2 <= '0;
	else if (rd_en) begin
            if (rd_addr2 == 0)
                rd_data2 <= 0;
            else
                rd_data2 <= x[rd_addr2];
        end

endmodule



module riscv(
    input wire clk,
    input wire rstn,

    output wire imem_req,
    output wire[31:0] imem_addr,
    input wire imem_rvalid,
    input wire[31:0] imem_rdata,
    input wire imem_gnt,
    
    output wire dmem_req,
    output wire[31:0] dmem_addr,
    output wire dmem_wren,
    output wire[7:0] dmem_wstrobe,
    output wire[63:0] dmem_wdata,
    input wire dmem_rvalid,
    input wire[63:0] dmem_rdata,
    input wire dmem_gnt,
    input wire in_MSIP,
    input wire in_MTIP,
    input wire in_MEIP,
    input wire in_HALTP
);

    typedef enum {
        RISCV_RESET = 1 << 0,
        RISCV_IF    = 1 << 1,
        RISCV_ID    = 1 << 2,
        RISCV_EX    = 1 << 3,
        RISCV_MEM   = 1 << 4,
        RISCV_WB    = 1 << 5
    } RISCV_STATE;

    RISCV_STATE state;
    RISCV_STATE state_nxt;
    wire[31:0] id_instruction = imem_rdata;
    reg[31:0] pc;
    wire wb_wren;
    wire[63:0] wb_wdata;
    wire[4:0] wb_dst;
    wire[63:0] ex_r1;
    wire[63:0] ex_r2;
    REGISTER_FILE register_file(
        .clk(clk), .rstn(rstn), 
        .wr_en(wb_wren), .wr_addr(wb_dst), .wr_data(wb_wdata),
        .rd_en(state == RISCV_ID),
        .rd_addr1(id_instruction[19:15]), .rd_data1(ex_r1),
        .rd_addr2(id_instruction[24:20]), .rd_data2(ex_r2)
    );


    reg[31:0] ex_instruction;

    reg ex_reg_op_reg; // rd = r1 op r2
    reg ex_reg_op_imm; // rd = r1 op imm_I
    reg ex_is_jalr; // rd = pc + 4, pc = r1 + imm_I
    reg ex_is_jal; // rd = pc + 4, pc = pc + imm_J
    reg ex_read_mem; // rd = mem[r1 + imm_I]
    reg ex_is_lui; // rd = imm_U
    reg ex_is_auipc; // rd = pc + imm_U
    reg ex_is_branch; // pc = r1 cmp r2 ? pc + imm_B : pc + 4
    reg ex_write_mem; // mem[r1 + imm_S] = r2
    reg ex_is_system;
    
    
    
    wire[63:0] ex_imm_I = {{52{ex_instruction[31]}}, ex_instruction[31:20]};
    wire[63:0] ex_imm_S = {{52{ex_instruction[31]}}, ex_instruction[31:25], ex_instruction[11:7]};
    wire[63:0] ex_imm_B = {{52{ex_instruction[31]}}, ex_instruction[7], ex_instruction[30:25], ex_instruction[11:8], 1'b0};
    wire[63:0] ex_imm_U = {{32{ex_instruction[31]}}, ex_instruction[31:12], {12{1'b0}}};
    wire[63:0] ex_imm_J = {{44{ex_instruction[31]}}, ex_instruction[19:12], ex_instruction[20], ex_instruction[30:21], 1'b0};

    reg[31:0] wb_pc;
    reg[63:0] wb_alu_result;
    reg[4:0] wb_rd;

    reg[31:0] mem_mem_addr;

    wire[63:0] ex_alu_result;
    wire[64:0] ex_sub_cmp = ex_r1 - ex_r2;

    wire ex_cmp_eq = ex_sub_cmp[63:0] == 64'h0;
    wire ex_cmp_lt = ex_r1[63] == ex_r2[63] ? ex_sub_cmp[64] : ex_r1[63];
    wire ex_cmp_ltu = ex_sub_cmp[64];
    reg ex_can_branch;
    always_comb
        case (ex_instruction[14:12])
            3'b000: ex_can_branch = ex_cmp_eq;
            3'b001: ex_can_branch = ~ex_cmp_eq;
            3'b100: ex_can_branch = ex_cmp_lt;
            3'b101: ex_can_branch = ~ex_cmp_lt;
            3'b110: ex_can_branch = ex_cmp_ltu;
            3'b111: ex_can_branch = ~ex_cmp_ltu;
            default: ex_can_branch = 1'bX;
        endcase


    reg[63:0] cycle_ctr;
    always @(posedge clk or negedge rstn)
        if (!rstn)
            cycle_ctr <= '0;
        else
            cycle_ctr <= cycle_ctr + 1;
    
    /* CSR HANDLING */

    typedef enum logic[11:0] {
        CSR_MCYCLE      = 12'hB00,
        CSR_MCYCLEH     = 12'hB80,

        CSR_CYCLE       = 12'hC00,
        CSR_CYCLEH      = 12'hC80,
        CSR_TIME        = 12'hC01,
        CSR_TIMEH       = 12'hC81,
        
        CSR_MVENDORID   = 12'hF11,
        CSR_MARCHID     = 12'hF12,
        CSR_MIMPID      = 12'hF13,
        CSR_MHARTID     = 12'hF14,

        CSR_MSTATUS     = 12'h300,
        CSR_MSTATUSH    = 12'h310,
        CSR_MISA        = 12'h301,
        CSR_MIE         = 12'h304,
        CSR_MTVEC       = 12'h305,
        
        CSR_MSCRATCH    = 12'h340,
        CSR_MEPC        = 12'h341,
        CSR_MCAUSE      = 12'h342,
        CSR_MTVAL       = 12'h343,
        CSR_MIP         = 12'h344,
        
        CSR_DCSR        = 12'h7b0,
        CSR_DPC         = 12'h7b1,
        CSR_DSCRATCH0   = 12'h7b2,
        CSR_DSCRATCH1   = 12'h7b3
    } CSR_t;

    reg is_debug_mode;
    reg csr_mstatus_mie;
    reg csr_mstatus_mpie;
    wire[1:0] csr_mstatus_mpp = 2'b11;
    
    
    wire[63:0] csr_mstatus = {
        csr_mstatus_mpp, 2'b00, 1'b0,
        csr_mstatus_mpie, 1'b0, 1'b0, 1'b0, csr_mstatus_mie, 1'b0, 1'b0, 1'b0
    };
    wire[63:0] csr_mip = {in_MEIP, 3'b000, in_MTIP, 3'b000, in_MSIP, 3'b000};
    reg[63:0] csr_mie;
    reg[63:0] csr_mtvec;
    reg[63:0] csr_mcause;
    reg[63:0] csr_mepc;
    reg[63:0] csr_mtval;
    reg[63:0] csr_dpc;

    wire[3:0] dcsr_xdebugver = 4;
    reg dcsr_ebreakm;
    reg[2:0] dcsr_cause;
    reg dcsr_step;
    wire[63:0] csr_dcsr = {
        dcsr_xdebugver, 12'd0, 
        dcsr_ebreakm, 1'b0, 1'b0, 1'b0,
        1'b0, 1'b0, 1'b0, dcsr_cause, 1'b0,
        1'b0, 1'b0, dcsr_step, 2'b11
    };
    wire[11:0] ex_csr_idx = ex_instruction[31:20];

    reg[63:0] csr_mscratch;
    reg[63:0] csr_dscratch0;
    reg[63:0] csr_dscratch1;
    wire[63:0] csr_misa = (64'h1 << 63) |   // XLEN = 64-bit
                          (64'h1 << 8);     // I "extension"
    reg[64:0] ex_csr_rdval; // bit 64: invalid csr, = 0 when the csr is valid

    wire wb_mtip_pending = csr_mip[7] & csr_mie[7] & csr_mstatus_mie & ~in_HALTP & ~dcsr_step & ~is_debug_mode;
    always_comb begin
        ex_csr_rdval = '1;
        case (ex_csr_idx)
            CSR_MCYCLE:      ex_csr_rdval = cycle_ctr;
            
            CSR_MVENDORID:  ex_csr_rdval = '0;
            CSR_MARCHID:    ex_csr_rdval = '0;
            CSR_MIMPID:     ex_csr_rdval = '0;
            CSR_MHARTID:    ex_csr_rdval = '0;
            CSR_MISA:       ex_csr_rdval = csr_misa;
            CSR_MIP:        ex_csr_rdval = csr_mip;
            
            CSR_MIE:        ex_csr_rdval = csr_mie;
            CSR_MTVEC:      ex_csr_rdval = csr_mtvec;
            
            CSR_MSCRATCH:   ex_csr_rdval = csr_mscratch;
            CSR_MEPC:       ex_csr_rdval = csr_mepc;
            CSR_MCAUSE:     ex_csr_rdval = csr_mcause;
            CSR_MTVAL:      ex_csr_rdval = csr_mtval;
            CSR_MSTATUS:    ex_csr_rdval = csr_mstatus;

            CSR_DCSR:       ex_csr_rdval = is_debug_mode ? csr_dcsr : '1;
            CSR_DPC:        ex_csr_rdval = is_debug_mode ? csr_dpc : '1;
            CSR_DSCRATCH0:  ex_csr_rdval = is_debug_mode ? csr_dscratch0 : '1;
            CSR_DSCRATCH1:  ex_csr_rdval = is_debug_mode ? csr_dscratch1 : '1;
        endcase
    end

    wire ex_csr_wren = ex_is_system && (ex_instruction[13:12] == 2'b01 || ex_instruction[13] && ex_instruction[19:15]);
    wire ex_csr_inst = ex_is_system && ex_instruction[13:12];
    reg ex_is_mret;
    reg ex_is_fence;
    reg ex_is_ecall;
    reg ex_is_ebreak;
    reg ex_is_dret;

    wire ex_csr_wr_illegal = ex_csr_idx[11:10] == 2'b11 && ex_csr_wren;
//                             cpu_cur_priv < ex_csr_idx[9:8];

    reg[63:0] wb_csr_wrdata;
    reg[31:0] wb_mem_addr;
    reg wb_csr_wren;
    reg wb_reg_write;
    reg[11:0] wb_csr_idx;

    wire[63:0] ex_csr_operand = ex_instruction[14] ? ex_instruction[19:15] : ex_r1;
    always @(posedge clk or negedge rstn)
	if (!rstn) begin
		wb_csr_wren <= 1'b0;
		wb_csr_idx <= '0;
		wb_csr_wrdata <= '0;
	end
        else if (state == RISCV_EX) begin
            wb_csr_wren <= ex_csr_wren;
            wb_csr_idx <= ex_csr_idx;
            case (ex_instruction[13:12])
                2'b00: wb_csr_wrdata <= 'x;
                2'b01: wb_csr_wrdata <= ex_csr_operand;
                2'b10: wb_csr_wrdata <= ex_csr_rdval[63:0] | ex_csr_operand; 
                2'b11: wb_csr_wrdata <= ex_csr_rdval[63:0] & ~ex_csr_operand;
            endcase
        end

    always @(posedge clk or negedge rstn)
        if (!rstn)
            csr_mie <= '0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_MIE)
            csr_mie <= wb_csr_wrdata & 32'h888;

    always @(posedge clk or negedge rstn)
        if (!rstn)
            csr_mtvec <= '0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_MTVEC)
            csr_mtvec <= {wb_csr_wrdata[63:2], 1'b0, wb_csr_wrdata[0]}; 

    always @(posedge clk or negedge rstn)
        if (!rstn)
            csr_mscratch <= '0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_MSCRATCH)
            csr_mscratch <= wb_csr_wrdata;

    always @(posedge clk or negedge rstn)
        if (!rstn)
            csr_dscratch0 <= '0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_DSCRATCH0)
            csr_dscratch0 <= wb_csr_wrdata;

    always @(posedge clk or negedge rstn)
        if (!rstn)
            csr_dscratch1 <= '0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_DSCRATCH1)
            csr_dscratch1 <= wb_csr_wrdata;
    reg wb_is_mret;
                          
    always @(posedge clk or negedge rstn)
	   if (!rstn) begin
 		ex_instruction <= '0;

                ex_reg_op_reg    <= '0; // rd = r1 op r2 -> wb
                ex_reg_op_imm    <= '0; // rd = r1 op imm_I -> wb
                ex_is_jalr       <= '0; // rd = pc + 4, pc = r1 + imm_I -> wb
                ex_is_jal        <= '0; // rd = pc + 4, pc = pc + imm_J -> wb
                ex_is_lui        <= '0; // rd = imm_U -> wb
                ex_is_auipc      <= '0; // rd = pc + imm_U -> wb

                // memory instruction
                ex_read_mem      <= '0; // rd = mem[r1 + imm_I] -> mem
                ex_write_mem     <= '0; // mem[r1 + imm_S] = r2 -> mem

                ex_is_branch     <= '0; // pc = r1 cmp r2 ? pc + imm_B : pc + 4
                ex_is_system     <= '0;
                ex_is_fence      <= '0;
                ex_is_dret       <= '0;
                ex_is_mret       <= '0;
                ex_is_ecall      <= '0;
                ex_is_ebreak     <= '0;
wb_pc <= '0;

wb_alu_result  <= '0;
mem_mem_addr <= '0;
                wb_reg_write <= '0;
                wb_rd <= '0;
                wb_is_mret <= '0;
                
                
                wb_mem_addr <= '0;
end else 
        case (state)
            RISCV_ID: begin
                
                ex_instruction <= id_instruction;

                ex_reg_op_reg    <= id_instruction[6:2] == 5'b01100 || id_instruction[6:2] == 5'b01110; // rd = r1 op r2 -> wb
                ex_reg_op_imm    <= id_instruction[6:2] == 5'b00100 || id_instruction[6:2] == 5'b00110; // rd = r1 op imm_I -> wb
                ex_is_jalr       <= id_instruction[6:2] == 5'b11001; // rd = pc + 4, pc = r1 + imm_I -> wb
                ex_is_jal        <= id_instruction[6:2] == 5'b11011; // rd = pc + 4, pc = pc + imm_J -> wb
                ex_is_lui        <= id_instruction[6:2] == 5'b01101; // rd = imm_U -> wb
                ex_is_auipc      <= id_instruction[6:2] == 5'b00101; // rd = pc + imm_U -> wb

                // memory instruction
                ex_read_mem      <= id_instruction[6:2] == 5'b00000; // rd = mem[r1 + imm_I] -> mem
                ex_write_mem     <= id_instruction[6:2] == 5'b01000; // mem[r1 + imm_S] = r2 -> mem

                ex_is_branch     <= id_instruction[6:2] == 5'b11000; // pc = r1 cmp r2 ? pc + imm_B : pc + 4
                ex_is_system     <= id_instruction[6:2] == 5'b11100;
                ex_is_fence      <= id_instruction[6:2] == 5'b00011;
                ex_is_dret       <= id_instruction == 32'h7b200073;
                ex_is_mret       <= id_instruction == 32'h30200073;
                ex_is_ecall      <= id_instruction == 32'h00000073;
                ex_is_ebreak     <= id_instruction == 32'h00100073;
            end
            RISCV_EX: begin
                wb_pc <= pc + 4;
//                if (ex_reg_op_reg | ex_reg_op_imm)
                    wb_alu_result <= ex_alu_result;

                if (ex_is_jalr) begin
                    wb_alu_result <= pc + 4;
                    wb_pc <= ex_r1 + {ex_imm_I[63:1], 1'b0};
                end

                if (ex_is_jal) begin
                    wb_alu_result <= pc + 4;
                    wb_pc <= pc + ex_imm_J;
                end
                if (ex_is_auipc | ex_is_lui)
                    wb_alu_result <= (ex_is_auipc ? pc : 0) + ex_imm_U;

                if (ex_can_branch & ex_is_branch)
                    wb_pc <= pc + ex_imm_B;
                if (ex_csr_inst)
                    wb_alu_result <= ex_csr_rdval[63:0];
                if (ex_is_mret)
                    wb_pc <= csr_mepc;
                if (ex_is_dret)
                    wb_pc <= csr_dpc;
                if (ex_is_ebreak)
                    wb_pc <= pc;
                mem_mem_addr <= ex_r1 + (ex_write_mem ? ex_imm_S : ex_imm_I);
                wb_reg_write <= (ex_reg_op_reg | ex_reg_op_imm | ex_is_jalr | ex_is_jal | ex_is_lui | ex_is_auipc | ex_read_mem | ex_csr_inst);
                wb_rd <= ex_instruction[11:7];
                wb_is_mret <= ex_is_mret;
                
                
            end
            RISCV_MEM: begin
                wb_mem_addr <= mem_mem_addr;
            end
        endcase

    
    ALU_RISCV alu0(
        .result(ex_alu_result), .operand_1(ex_r1), .operand_2(ex_reg_op_imm ? ex_imm_I : ex_r2),
        .instruction(ex_instruction)
    );
    wire[63:0] wb_mstatus = wb_csr_idx == CSR_MSTATUS && wb_csr_wren ? wb_csr_wrdata : 
                            wb_is_mret                               ? {
                                                                            2'b00, 2'b00, 1'b0,
                                                                            1'b1, 1'b0, 1'b0, 1'b0, csr_mstatus_mpie, 1'b0, 1'b0, 1'b0
                                                                        } : 
                                                                       csr_mstatus;


    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            csr_mepc <= 0;
            csr_mcause <= 0;
            csr_mtval <= 0;
        end else if (state == RISCV_WB)
            if (wb_mtip_pending) begin
                csr_mepc <= wb_pc;
                csr_mcause <= 64'h8000_0000_0000_0007;
                csr_mtval <= 0;
            end else if (wb_csr_wren)
                case (wb_csr_idx)
                    CSR_MEPC: csr_mepc <= {wb_csr_wrdata[63:2], 2'b00};
                    CSR_MCAUSE: csr_mcause <= wb_csr_wrdata;
                    CSR_MTVAL: csr_mtval <= wb_csr_wrdata;
                endcase
    
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            dcsr_step <= 1'b0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_DCSR && is_debug_mode)
            dcsr_step <= wb_csr_wrdata[2];
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            dcsr_ebreakm <= 1'b0;
        else if (state == RISCV_WB && wb_csr_wren && wb_csr_idx == CSR_DCSR && is_debug_mode)
            dcsr_ebreakm <= wb_csr_wrdata[15];     
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin begin
            csr_dpc <= '0;
            dcsr_cause <= '0;
            is_debug_mode <= 1'b0;
        end
        end else if (state == RISCV_WB)
            if (!is_debug_mode) begin
                if (in_HALTP) begin
                    csr_dpc <= wb_pc;
                    dcsr_cause <= 3;
                    is_debug_mode <= 1'b1;
                end else if (dcsr_step) begin
                    csr_dpc <= wb_pc;
                    dcsr_cause <= 4;
                    is_debug_mode <= 1'b1;
                end else if (ex_is_ebreak && dcsr_ebreakm) begin
                    csr_dpc <= wb_pc;
                    dcsr_cause <= 1;
                    is_debug_mode <= 1'b1;
                end
            end else begin 
                if (ex_is_dret)
                    is_debug_mode <= 1'b0;
                if (wb_csr_wren && wb_csr_idx == CSR_DPC)   
                    csr_dpc <= {wb_csr_wrdata[63:2], 2'b00};
            end
            
        

    // mstatus
    always @(posedge clk or negedge rstn)
        if (!rstn) begin
            csr_mstatus_mpie <= 0;
            csr_mstatus_mie <= 0;
        end else if (state == RISCV_WB) begin
            csr_mstatus_mpie <= wb_mstatus[7];
            csr_mstatus_mie <= wb_mstatus[3];
            if (wb_mtip_pending) begin
                csr_mstatus_mpie <= wb_mstatus[3];
                csr_mstatus_mie <= 1'b0;
            end
        end
    
    reg[7:0] base_wstrobe;
    always_comb
        unique case (ex_instruction[13:12])
            2'b11: base_wstrobe = 8'hFF;
            2'b10: base_wstrobe = 8'h0F;
            2'b01: base_wstrobe = 8'h03;
            2'b00: base_wstrobe = 8'h01;
        endcase

    
    reg[7:0] omem_wstrobe;
    reg[63:0] omem_wdata;
    always_comb begin
            omem_wstrobe = base_wstrobe << mem_mem_addr[2:0];
            
            case (ex_instruction[13:12])
                2'b11: omem_wdata = ex_r2;
                2'b10: omem_wdata = {2{ex_r2[31:0]}};
                2'b01: omem_wdata = {4{ex_r2[15:0]}};
                2'b00: omem_wdata = {8{ex_r2[7:0]}};
            endcase

    end
    
    wire rd_mem_sext = ~ex_instruction[14];
    reg[63:0] wb_rd_mem_data;

    wire[63:0] shifted_data = dmem_rdata >> 8*wb_mem_addr[2:0];
    always_comb
        unique case (ex_instruction[13:12])
            2'b11: wb_rd_mem_data = shifted_data;
            2'b10: wb_rd_mem_data = {{32{shifted_data[31] & rd_mem_sext}}, shifted_data[31:0]};
            2'b01: wb_rd_mem_data = {{48{shifted_data[15] & rd_mem_sext}}, shifted_data[15:0]};
            2'b00: wb_rd_mem_data = {{56{shifted_data[7] & rd_mem_sext}}, shifted_data[7:0]};
        endcase
    always_comb begin
        state_nxt = state;
        case (state)
            RISCV_RESET: state_nxt = RISCV_IF;
            RISCV_IF: if (imem_req & imem_gnt) state_nxt = RISCV_ID;
            RISCV_ID: if (imem_rvalid) state_nxt = RISCV_EX;
            RISCV_EX: state_nxt = (ex_read_mem | ex_write_mem) ? RISCV_MEM : RISCV_WB;
            RISCV_MEM: if (dmem_req & dmem_gnt) state_nxt = RISCV_WB;
            RISCV_WB: if (!ex_read_mem && !ex_write_mem || dmem_rvalid) state_nxt = RISCV_IF;
        endcase
    end
    always @(posedge clk or negedge rstn)
        if (!rstn)
            pc <= 32'h10000;
        else if (state == RISCV_WB) begin
            if ((in_HALTP || dcsr_step || ex_is_ebreak && dcsr_ebreakm) && !is_debug_mode)
                pc <= 32'h700;
            else if (wb_mtip_pending)
                pc <= 4*(csr_mtvec[63:2] + 7*csr_mtvec[0]);
            else
                pc <= wb_pc;
        end
    assign wb_wren = state == RISCV_WB && wb_reg_write;
    assign wb_wdata = ex_read_mem ? wb_rd_mem_data : wb_alu_result;
    assign wb_dst = wb_rd;
    
    always @(posedge clk or negedge rstn)
        if (!rstn)
            state <= RISCV_RESET;
        else
            state <= state_nxt;
    
    assign imem_req  = state == RISCV_IF;
    assign dmem_req  = state == RISCV_MEM;
    assign imem_addr = pc;
    assign dmem_addr = mem_mem_addr;
    assign dmem_wdata = omem_wdata;
    assign dmem_wstrobe = omem_wstrobe;
    assign dmem_wren = ex_write_mem;
endmodule

