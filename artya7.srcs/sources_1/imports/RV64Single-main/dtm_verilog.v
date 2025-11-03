`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 03/27/2025 12:18:53 PM
// Design Name: 
// Module Name: top
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

`default_nettype none
module dtm_verilog(
    input wire clk_i,
    input wire rstn_i,

    input wire jtag_trst_i,
    input wire jtag_tck_i,
    input wire jtag_tdi_i,
    input wire jtag_tms_i,
    output reg jtag_tdo_o,

    output wire dmi_rstn_o,
    output wire dmi_req_valid_o,
    input wire dmi_req_ready_i,
    output wire[6:0] dmi_req_addr_o,
    output wire dmi_req_op_o,
    output wire[31:0] dmi_req_data_o,
    input wire dmi_resp_valid_i,
    input wire[31:0] dmi_resp_data_i,
    input wire dmi_resp_err_i
);
    wire clk = clk_i;
    wire rstn = rstn_i;
    parameter IDCODE_VERSION = 0;
    parameter IDCODE_MANID = 0;
    parameter IDCODE_PARTID = 0;
    localparam DEBUG_SPEC = 4'b0001;
    localparam DEBUG_ABITS = 6'd7;
    reg[3:0] jtag_tck_sync;
    reg[3:0] jtag_tms_sync;
    reg[3:0] jtag_tdi_sync;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            jtag_tck_sync <= '0;
            jtag_tms_sync <= '0;
            jtag_tdi_sync <= '0;
        end else begin
            jtag_tck_sync <= {jtag_tck_i, jtag_tck_sync[3:1]};
            jtag_tms_sync <= {jtag_tms_i, jtag_tms_sync[3:1]};
            jtag_tdi_sync <= {jtag_tdi_i, jtag_tdi_sync[3:1]};
        end
    wire jtag_tck_rising = jtag_tck_sync[1:0] == 2'b10;
    wire jtag_tck_falling = jtag_tck_sync[1:0] == 2'b01;
    
    reg[4:0] ir_reg;
    wire jtag_tms_val = jtag_tms_sync[1];
    wire jtag_tdi_val = jtag_tdi_sync[1];
    
    typedef enum {
        JTAG_RESET,
        JTAG_IDLE,
        JTAG_DR_SCAN,
        JTAG_CAPTURE_DR,
        JTAG_SHIFT_DR,
        JTAG_EXIT1_DR,
        JTAG_PAUSE_DR,
        JTAG_EXIT2_DR,
        JTAG_UPDATE_DR,
        JTAG_IR_SCAN,
        JTAG_CAPTURE_IR,
        JTAG_SHIFT_IR,
        JTAG_EXIT1_IR,
        JTAG_PAUSE_IR,
        JTAG_EXIT2_IR,
        JTAG_UPDATE_IR
        
    } JTAG_STATE;
    JTAG_STATE jtag_state;
    JTAG_STATE jtag_state_prev;

     typedef enum {
        DMI_IDLE,
        DMI_READ_WAIT,
        DMI_READ_BUSY,
        DMI_WRITE_WAIT,
        DMI_WRITE_BUSY
        
    } DMI_STATE;
    DMI_STATE dmi_state;
    reg dmi_hard_reset;
    reg dmi_reset;
    reg[31:0] dmi_rdata;
    reg[31:0] dmi_wdata;
    reg[6:0] dmi_addr;

    reg[40:0] shift_out_dmi;
    reg[31:0] shift_out_dtmcs;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            dmi_state <= DMI_IDLE;
            dmi_hard_reset <= 1'b1;
            dmi_reset <= 1'b1;
            dmi_rdata <= '0;
            dmi_wdata <= '0;
            dmi_addr <= '0;
        end else begin
            dmi_hard_reset <= 1'b0;
            dmi_reset <= 1'b0;
            if (jtag_state == JTAG_UPDATE_DR && jtag_state_prev != JTAG_UPDATE_DR && ir_reg == 5'b10000) begin
                dmi_reset <= shift_out_dtmcs[16];
                dmi_hard_reset <= shift_out_dtmcs[17];
            end
            if (dmi_hard_reset) begin
                dmi_state <= DMI_IDLE;
            end else
                case (dmi_state)
                    DMI_IDLE: 
                        if (jtag_state == JTAG_UPDATE_DR && jtag_state_prev != JTAG_UPDATE_DR && ir_reg == 5'b10001) begin
                            if (shift_out_dmi[1:0] == 2'b01)
                                dmi_state <= DMI_READ_WAIT;
                            else if (shift_out_dmi[1:0] == 2'b10)
                                dmi_state <= DMI_WRITE_WAIT;
                            dmi_addr <= shift_out_dmi[40:34];
                            dmi_wdata <= shift_out_dmi[33:2];
                        end
                    DMI_READ_WAIT:
                       if (dmi_req_ready_i)
                            dmi_state <= DMI_READ_BUSY;
                    DMI_READ_BUSY:
                        if (dmi_resp_valid_i) begin
                            dmi_rdata <= dmi_resp_data_i;
                            dmi_state <= DMI_IDLE;
                        end
                    DMI_WRITE_WAIT:
                        if (dmi_req_ready_i)
                            dmi_state <= DMI_WRITE_BUSY;
                    DMI_WRITE_BUSY:
                        if (dmi_resp_valid_i) begin
                            dmi_state <= DMI_IDLE;
                        end
                    default:
                        dmi_state <= DMI_IDLE;
                    
                endcase
        end


    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            jtag_state <= JTAG_RESET;
            jtag_state_prev <= JTAG_RESET;
        end else begin   
            jtag_state_prev <= jtag_state;
            if (jtag_tck_rising) begin
                case (jtag_state)
                    JTAG_RESET:         jtag_state <= jtag_tms_val ? JTAG_RESET : JTAG_IDLE;
                    JTAG_IDLE:          jtag_state <= jtag_tms_val ? JTAG_DR_SCAN : JTAG_IDLE;
                    
                    JTAG_DR_SCAN:       jtag_state <= jtag_tms_val ? JTAG_IR_SCAN : JTAG_CAPTURE_DR;
                    JTAG_CAPTURE_DR:    jtag_state <= jtag_tms_val ? JTAG_EXIT1_DR : JTAG_SHIFT_DR;
                    JTAG_SHIFT_DR:      jtag_state <= jtag_tms_val ? JTAG_EXIT1_DR : JTAG_SHIFT_DR;
                    JTAG_EXIT1_DR:      jtag_state <= jtag_tms_val ? JTAG_UPDATE_DR : JTAG_PAUSE_DR;
                    JTAG_PAUSE_DR:      jtag_state <= jtag_tms_val ? JTAG_EXIT2_DR : JTAG_PAUSE_DR;
                    JTAG_EXIT2_DR:      jtag_state <= jtag_tms_val ? JTAG_UPDATE_DR : JTAG_SHIFT_DR;
                    JTAG_UPDATE_DR:     jtag_state <= jtag_tms_val ? JTAG_DR_SCAN : JTAG_IDLE;
                    
                    JTAG_IR_SCAN:       jtag_state <= jtag_tms_val ? JTAG_RESET : JTAG_CAPTURE_IR;
                    JTAG_CAPTURE_IR:    jtag_state <= jtag_tms_val ? JTAG_EXIT1_IR : JTAG_SHIFT_IR;
                    JTAG_SHIFT_IR:      jtag_state <= jtag_tms_val ? JTAG_EXIT1_IR : JTAG_SHIFT_IR;
                    JTAG_EXIT1_IR:      jtag_state <= jtag_tms_val ? JTAG_UPDATE_IR : JTAG_PAUSE_IR;
                    JTAG_PAUSE_IR:      jtag_state <= jtag_tms_val ? JTAG_EXIT2_IR : JTAG_PAUSE_IR;
                    JTAG_EXIT2_IR:      jtag_state <= jtag_tms_val ? JTAG_UPDATE_IR : JTAG_SHIFT_IR;
                    JTAG_UPDATE_IR:     jtag_state <= jtag_tms_val ? JTAG_DR_SCAN : JTAG_IDLE;
                endcase
            end
        end
    localparam IDCODE = 32'h0000_0001;
   

    reg shift_out_bypass;
    reg[31:0] shift_out_idcode;
    wire[40:0] dmi_nxt = {dmi_addr, dmi_rdata, 2'b00};
    wire[31:0] dtmcs_nxt = {20'd0, dmi_nxt[1:0], DEBUG_ABITS, DEBUG_SPEC};
    reg shift_out_tdo;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn) begin
            ir_reg <= 5'b00001;
            shift_out_idcode <= IDCODE;
            shift_out_dtmcs <= '0;
            shift_out_dmi <= '0;
            shift_out_bypass <= 1'b0;
            shift_out_tdo <= 1'b0;
        end else begin
            if (jtag_tck_rising)
                case (jtag_state)
                    JTAG_RESET: ir_reg <= 5'b00001;
                    JTAG_CAPTURE_IR: ir_reg <= 5'b00001;
                    JTAG_SHIFT_IR: ir_reg <= {jtag_tdi_val, ir_reg[4:1]};
                    JTAG_CAPTURE_DR:
                        case (ir_reg)
                            5'b00001: shift_out_idcode <= IDCODE;
                            5'b10000: shift_out_dtmcs <= dtmcs_nxt;
                            5'b10001: shift_out_dmi <= dmi_nxt;
                            default: shift_out_bypass <= 1'b0;
                        endcase
                    JTAG_SHIFT_DR:
                        case (ir_reg)
                            5'b00001: shift_out_idcode <= {jtag_tdi_val, shift_out_idcode[31:1]};
                            5'b10000: shift_out_dtmcs <= {jtag_tdi_val, shift_out_dtmcs[31:1]};
                            5'b10001: shift_out_dmi <= {jtag_tdi_val, shift_out_dmi[40:1]};
                            default: shift_out_bypass <= jtag_tdi_val;
                        endcase
                endcase
            
            if (jtag_state == JTAG_SHIFT_IR)
                shift_out_tdo <= ir_reg[0];
            else
                case (ir_reg)
                    5'b00001: shift_out_tdo <= shift_out_idcode[0];
                    5'b10000: shift_out_tdo <= shift_out_dtmcs[0];
                    5'b10001: shift_out_tdo <= shift_out_dmi[0];
                    default: shift_out_tdo <= shift_out_bypass;
                endcase
        end
    reg jtag_tdo_q;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            jtag_tdo_q <= 1'b0;
        else if (jtag_tck_falling)
            jtag_tdo_q <= shift_out_tdo;
    always_ff @(posedge clk or negedge rstn)
        if (!rstn)
            jtag_tdo_o <= 1'b0;
        else
            jtag_tdo_o <= jtag_tdo_q;
    assign dmi_rstn_o = dmi_hard_reset ? 1'b0 : 1'b1;
    assign dmi_req_valid_o = dmi_state == DMI_READ_WAIT || dmi_state == DMI_WRITE_WAIT;
    assign dmi_req_op_o = dmi_state == DMI_WRITE_WAIT;
    assign dmi_req_addr_o = dmi_addr;
    assign dmi_req_data_o = dmi_wdata;
endmodule
