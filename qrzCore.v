/* 
 * A very simple Forth in Verilog
 * (c) 2013, 2014 R.A. Paz Schmidt
 * 16 bit opcodes/Addresses
 * External Program memory
 * External Data Stack Memory
 * External Return Stack Memory
 * 4 Clocks Machine cycle
 * - Fetch Opcode
 * - Do something
 * - Read or Write Memory
 * - Write back Stack Pointer
 * - Debug console
 *   g(o)
 *   h(alt)
 *   r(eset)
 *   s(single step)
 *   t(race)
 */
`default_nettype none 
module qrzCore(
	input wire clk40,
	input wire sys_clk,
	input wire resetn_i,
	output wire [15:0] cpu_addr_o,
	output wire cpu_oen_o,
	output wire cpu_wen_o,
	output wire cpu_cen_o,
	input wire [15:0] cpu_data_i,
	output wire [15:0] cpu_data_o,
	output wire [2:0] cpu_state_o,
	output wire [15:0] cpu_debug_data_o,
	input wire rxd_i,
	output wire txd_o
	);
	
assign cpu_cen_o = cpu_wen_o | cpu_oen_o; 

/*
 * Instructions
 *
 * 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
 *  0 <---------- 15 bit constant push ---------->   
 *  1  0  0  x  x  x  x  x  x  x  0  0  0  0  0  0   8000  CMPZ  A = A == 0
 *  1  0  0  x  x  x  x  x  x  x  0  0  0  0  0  1   8001  CMPNZ A = A != 0
 *  1  0  0  N  N  N  N  N  N  N  0  0  0  0  1  0   8002  N+    A = A + N
 *  1  0  0  N  N  N  N  N  N  N  0  0  0  0  1  1   8003  N-    A = A - N
 *  1  0  0  x  x  x  x  x  x  x  0  0  1  0  0  0   8008  NOT   A = !A
 *  1  0  0  x  x  x  x  x  x  x  0  0  1  1  0  0   800C  ROL   A = {c} A >> 1
 *  1  0  0  x  x  x  x  x  x  x  0  0  1  1  0  1   800D  ROR   A = A << 1{c}
 *  1  0  0  x  x  x  x  x  x  x  0  0  1  1  1  0   800E  SHL   A = A << 1
 *  1  0  0  x  x  x  x  x  x  x  0  0  1  1  1  1   800F  ASR   A = A >> 1

 *  1  0  0  x  x  x  x  x  x  x  0  1  0  0  0  0   8010  ADD A = A + B
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  0  0  1   8011  SUB A = A - B
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  0  1  0   8012  AND A = A & B
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  0  1  1   8013  OR  A = A | B
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  1  0  0   8014  XOR A = A ^ B
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  1  0  1   8015  =
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  1  1  0   8016  !=  
 *  1  0  0  x  x  x  x  x  x  x  0  1  0  1  1  1   8017  >  
 *  1  0  0  x  x  x  x  x  x  x  0  1  1  0  0  0   8018  <

 *  1  0  0  x  x  x  x  x  x  x  1  0  0  0  0  0   8020  >r 
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  0  0  1   8021  r> 
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  0  1  0   8022  dup 
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  0  1  1   8023  drop 
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  1  0  0   8024  swap ( x1 x2 -- x2 x1 )
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  1  0  1   8025  over ( x1 x2 -- x1 x2 x1 )
 *  1  0  0  x  x  x  x  x  x  x  1  0  0  1  1  0   8026  roll ( x1 x2 x3 -- x2 x3 x1 )
 
 *  1  0  0  x  x  x  x  x  x  x  1  0  1  1  1  0   802E  push size data stack 
 *  1  0  0  x  x  x  x  x  x  x  1  0  1  1  1  1   802F  push size return stack 
 
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  0  0   8030  mem write a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  0  1   8031  mem write and inc addr a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  1  0   8032  mem read a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  1  1   8033  mem read and inc addr a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  0  0   8034  mem write b
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  0  1   8035  mem write and inc addr b
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  1  0   8036  mem read b
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  1  1   8037  mem read and inc addr b
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  0  1   8038  mem write and dec addr a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  0  1  1   8039  mem read and dec addr a
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  0  1   803A  mem write and dec addr b
 *  1  0  0  x  x  x  x  x  x  x  1  1  0  1  1  1   803B  mem read and dec addr b
 *  1  0  0  x  x  x  x  x  x  x  1  1  1  1  0  1   803C  mult  
 *  1  0  0  N  N  N  N  N  N  N  1  1  1  1  0  1   803D  pop to reg
 *  1  0  0  N  N  N  N  N  N  N  1  1  1  1  1  0   803E  push from reg
 *  1  0  0  x  x  x  x  x  x  x  1  1  1  1  1  1   803F  rts

 *  1  0  1  a  a  a  a  a  a  a  a  a  a  a  a  a   A/B000 jmpz
 *  1  1  0  a  a  a  a  a  a  a  a  a  a  a  a  a   C/D000 call
 *  1  1  1  a  a  a  a  a  a  a  a  a  a  a  a  a   E/F000 JMP
  *
 */

reg mem_read, mem_write;
reg [15:0] regpc, opcode, data_o, addr_o, tdreg, regpcp1;

/* HW registers */

reg [15:0] hwreg_addr_a;	// 0x00
reg [15:0] hwreg_addr_b;	// 0x01

reg [15:0] hwreg_dbg_pc;	// 0x08	823D 823E 100|0 00 10.00| 11 1101
reg [15:0] hwreg_dbg_c;		// 0x09	827D 827E 100|0 00 10.01| 11 1101
reg [15:0] hwreg_dbg_aa;	// 0x0A 82BD 82BE 100|0 00 10.10| 11 1101
reg [15:0] hwreg_dbg_ab;	// 0x0B 82FD 82FE 100|0 00 10.11| 11 1101
reg [15:0] hwreg_dbg_ss;	// 0x0B 833D 833E 100|0 00 11.00| 11 1101

reg [15:0] hwreg_ti_cnt_hi;	// 0x10
reg [15:0] hwreg_ti_cnt_lo;	// 0x11


reg [15:0] hwreg_sda_cnt;	// 0x18
reg [15:0] hwreg_sdb_cnt;	// 0x19

reg [7:0] hwreg_uart_txd;	// 0x20 883D 883E 100|0 10 00 00| 11 1101
wire hwreg_uart_txf;	    // 0x21 887D 887E 100|0 10 00 01| 11 1101
wire [7:0] hwreg_uart_rxd;	// 0x22
wire hwreg_uart_rxf;	    // 0x23


reg regc;
wire [3:0] retsp;
wire [4:0] datasp;
reg [2:0] state;
wire [15:0] alu_out;
wire alu_carry;
reg decode_read_reg;
reg decode_write_reg;
reg decode_jsr;
reg decode_rts;
reg decode_alu;
reg decode_imm;
reg decode_immhi;
reg decode_jmp;
reg decode_jmpz;
reg decode_mem_read_a;
reg decode_mem_write_a;
reg decode_inc_addr_a;
reg decode_dec_addr_a;
reg decode_mem_read_b;
reg decode_mem_write_b;
reg decode_inc_addr_b;
reg decode_dec_addr_b;
reg decode_poprs, decode_pushrs;
reg decode_popds, decode_pushds;
reg decode_push_dsp;
reg decode_push_rsp, decode_swap, decode_over, decode_roll;
reg decode_mult;
reg decode_save_top;
reg decode_rfd;
reg early_decode_dec_addr_a;
reg early_decode_dec_addr_b;
reg early_decode_mem_read_a;
reg early_decode_mem_read_b;
		
wire [15:0] TT, NN;
wire [15:0] SS; /* top and next */
wire [15:0] data_i;
wire tt_is_zero;
wire [15:0] regaddr_a_m1, regaddr_b_m1;

/* ICE debug support */
reg [2:0] dbg_state;
reg dbg_released;
reg [15:0] dbg_addr, dbg_data;

/* Debug support 
 *
 * When debug_active we are in debug mode
 * when debug_step an instruction will be executed
 */
reg debug_active, debug_step;

/* CPU I/O */
assign cpu_oen_o = !mem_read;
assign cpu_wen_o = !mem_write;
assign cpu_data_o = data_o;
assign data_i = cpu_data_i;
assign cpu_addr_o = addr_o;
assign cpu_state_o = state;
assign cpu_debug_data_o =  { 13'h0, hwreg_uart_ld_tx_req, hwreg_uart_tx_empty, hwreg_uart_tx_ack };

assign regaddr_a_m1 = hwreg_addr_a - 16'h1;
assign regaddr_b_m1 = hwreg_addr_b - 16'h1;

assign tt_is_zero = TT == 16'h0;

wire [31:0] mult_result;
reg mult_enabled;
wire mult_not_ready; // assrted as long as the multiplication needs

wire tx_baud_clk, rx_baud_clk; // rx & tx clocks 
wire hwreg_uart_tx_empty, hwreg_uart_rx_empty, hwreg_uart_tx_ack;
reg hwreg_uart_ld_tx_req, hwreg_uart_ld_rx_req;
wire [6:0] ireg_addr;
wire [15:0] to_top, to_top_rs; // data/return stack input muxes

boothmult mult(sys_clk, mult_enabled, NN, TT, mult_result, mult_not_ready);

alu alu(
	.clk(sys_clk),
	.opcode(opcode[5:0]),
	.ii(opcode[12:6]),
	.NN(NN),
	.TT(TT),
	.regc(regc),
	.alu_out_o(alu_out),
	.alu_carry_o(alu_carry) );

// data mux


assign to_top = decode_alu ? alu_out:
				decode_mult ? mult_result[15:0]:
				(decode_mem_read_a | decode_mem_read_b) ? data_i: // latches data from memory
				tdreg; // the rest
assign to_top_rs = decode_pushrs ? TT:
                   regpcp1;

data_stack dstack(
	.clk(sys_clk),
	.state(state), 			// 
	.push(decode_pushds),  	// this signal can be active together with save_xx
	.pop(decode_popds),  	// this signal can be active together with save_xx
	.swap(decode_swap),
	.roll(decode_roll),
	.over(decode_over),
	.save_dsp(decode_push_dsp),
	.save_top(decode_save_top),
	.save_next(decode_mult),
	.to_top(to_top), // data to be saved on the top of the stack
	.to_next(mult_result[31:16]), // data to be saved on the next of the stack
	.NN(NN), // next
	.TT(TT), // top of the stack
	.datasp(datasp) // stack pointer
	);

return_stack rstack(
	.clk(sys_clk),
	.state(state), 			// 
	.push(decode_pushrs | decode_jsr),  	// this signal can be active together with save_xx
	.pop(decode_poprs | decode_rts),  	// this signal can be active together with save_xx
	.save_top(decode_pushrs | decode_jsr),
	.to_top(to_top_rs), // data to be saved on the top of the stack
	.SS(SS), // top of the stack
	.retsp(retsp) // stack pointer
	);

brg baudrategen(.clk(clk40), .reset(!resetn_i), .tx_baud_clk(tx_baud_clk), .rx_baud_clk(rx_baud_clk));

uart uart0(.clk(clk40), .reset(!resetn_i),
	    .txclk(tx_baud_clk), 
		.ld_tx_req(hwreg_uart_ld_tx_req), 
		.ld_tx_ack(hwreg_uart_tx_ack), 
		.tx_data(hwreg_uart_txd), 
		.tx_enable(1'b1), 
		.tx_out(txd_o), 
		.tx_empty(hwreg_uart_tx_empty),
	    .rxclk(rx_baud_clk), 
		.uld_rx_req(hwreg_uart_ld_rx_req), 
		.uld_rx_ack(), 
		.rx_data(hwreg_uart_rxd), 
		.rx_enable(1'b1), 
		.rx_in(rxd_i), 
		.rx_empty(hwreg_uart_rx_empty));

assign hwreg_uart_txf = hwreg_uart_tx_empty | (!hwreg_uart_ld_tx_req); // 1 when empty
assign hwreg_uart_rxf = !hwreg_uart_rx_empty;


/* State machine */
`define ST_RESET		0
`define ST_COLD		  	1 /* used by the debugger to write to memory */
`define ST_WARM			2 /* used by the debugger to write to memory */
`define ST_PREFETCH		3
`define ST_FETCH		4
`define ST_DECODE		5
`define ST_MEM			6
`define ST_WBACK		7


assign ireg_addr = opcode[12:6];

always @(posedge sys_clk)
	begin
		if (resetn_i == 1'b0)
			state <= `ST_RESET;
		else
		case (state)
			`ST_RESET:
				state <= `ST_COLD;
			`ST_COLD:
				state <= `ST_WARM;
			`ST_WARM:
				state <= `ST_PREFETCH;
			`ST_PREFETCH:
				begin
					regpc <= 16'h0;
					hwreg_addr_a <= 16'h0000;
					hwreg_addr_b <= 16'h0550;
					addr_o <= 16'h0;
					debug_active <= 0; // active when in debug exception 
					debug_step <= 0; // single step
					if (debug_step)
						begin
							regpc <= 16'h0010; // debug exception
							debug_active <= 1; // cleared when rfd is executed
						end
					state <= `ST_FETCH;
				end
			`ST_FETCH:
				begin
					opcode <= data_i;
					state <= `ST_DECODE;
					regpcp1 <= regpc + 16'h1;
					if (early_decode_mem_read_a)
						begin
							if (early_decode_dec_addr_a) // pre decrement
								begin
									hwreg_addr_a <= regaddr_a_m1;
									addr_o <= regaddr_a_m1;
								end
							else
								addr_o <= hwreg_addr_a;
						end
					else
						if (early_decode_dec_addr_b)
							begin
								hwreg_addr_b <= regaddr_b_m1;
								addr_o <= regaddr_b_m1;
							end
						else
							addr_o <= hwreg_addr_b;
				end
			`ST_DECODE:
				begin
					state <= `ST_MEM;
					if (decode_mem_write_a)
						addr_o <= regaddr_a_m1;
					else
						if (decode_mem_write_b)
							addr_o <= hwreg_addr_b;
							
					if (decode_rts)
						regpc <= SS;
					else
						if (decode_jmp | decode_jsr | (decode_jmpz & tt_is_zero))
							regpc <= { 3'h0, opcode[12:0] };//{ 4'h0, opcode[12:1] };
						else
							regpc <= regpcp1;
					
				// pre-data mux, NN is the default (used for two-args ALU
				if (decode_imm)
					tdreg <= opcode[15:0];
				else
					if (decode_poprs) // r>
						tdreg <= SS;
					else
						if (decode_push_rsp)
							tdreg <= { 11'h0, retsp };
						else
							if (decode_read_reg)
								case (ireg_addr)
									0: tdreg <= hwreg_addr_a;
									1: tdreg <= hwreg_addr_b;
									8: tdreg <= hwreg_dbg_pc;
									9: tdreg <= hwreg_dbg_c;
									10: tdreg <= hwreg_dbg_aa;
									11: tdreg <= hwreg_dbg_ab;
									12: tdreg <= hwreg_dbg_ss;
									16: tdreg <= hwreg_ti_cnt_hi;
									17: tdreg <= hwreg_ti_cnt_lo;
									6'h21: tdreg <= { 14'h0, hwreg_uart_tx_empty & (!hwreg_uart_ld_tx_req) & (!hwreg_uart_tx_ack) };//{ 15'h0, hwreg_uart_txf }; // tx flags
									6'h22: tdreg <= { 8'h0, hwreg_uart_rxd };
									6'h23: tdreg <= { 15'h0, hwreg_uart_rxf }; // rx flags
								endcase
				end
			`ST_MEM: // data is being read-out from memory
				begin
					if (!(decode_mult & mult_not_ready)) // remains here till mult is done...
						state <= `ST_WBACK;
					/* debug */
					if (debug_step)
						begin
							if (debug_active)
								begin
									if (decode_rfd)
										begin
											debug_active <= 0;
											regpc <= hwreg_dbg_pc;
											regc <= hwreg_dbg_c;
											addr_o <= hwreg_dbg_pc;
										end
									else
										addr_o <= regpc; // normal execution of debug exception
								end
							else // debug not active but single step, go to debug
								begin
									hwreg_dbg_pc <= regpc; // for debug
									hwreg_dbg_c <= regc;
									hwreg_dbg_aa <= hwreg_addr_a;
									hwreg_dbg_ab <= hwreg_addr_b;
									hwreg_dbg_ss <= SS;
									debug_active <= 1;
									regpc <= 16'h0010; // debug exception
									addr_o <= 16'h0010; // debug exception
								end
						end
					else // not in debug mode
						addr_o <= regpc;
					if (decode_write_reg)
						case (ireg_addr)
							0: hwreg_addr_a <= TT;
							1: hwreg_addr_b <= TT;
							7'h20: begin hwreg_uart_txd <= TT[7:0]; hwreg_uart_ld_tx_req <= 1; end
						endcase
					if (decode_inc_addr_a)
						hwreg_addr_a <= hwreg_addr_a + 16'h1;
					if (decode_inc_addr_b)
						hwreg_addr_b <= hwreg_addr_b + 16'h1;
					if (decode_alu)
						regc <= alu_carry;
				end
			`ST_WBACK: // and latched here
				begin
					state <= `ST_FETCH;
				end
		endcase
		// acknowledge transmitted char
		if ((hwreg_uart_ld_tx_req == 1'b1) && (hwreg_uart_tx_ack == 1'b1)) // if byte accepted
			begin
				hwreg_uart_ld_tx_req <= 1'b0;
			end
		/* Reception */
		if (!hwreg_uart_rx_empty) // something was received...
			begin
				hwreg_uart_ld_rx_req <= 1; // load data to rx register...
			end
		if ((hwreg_uart_ld_rx_req) && (hwreg_uart_rx_empty)) // char received and acknowledged
			begin
				hwreg_uart_ld_rx_req <= 0;
			end
		{ hwreg_ti_cnt_hi, hwreg_ti_cnt_lo } <= { hwreg_ti_cnt_hi, hwreg_ti_cnt_lo } + 32'h1;
	end

/* Data and write/oe output */
always @(*)
	begin
		data_o = 16'hzz;
		mem_read = 0;
		mem_write = 0;
		//mult_enabled = 0;
		case (state)
			`ST_PREFETCH:
				begin
					mem_read = 1;
				end
			`ST_FETCH:
				begin
					mem_read = 1;
				end
			`ST_DECODE: // outputs addresses, read memory
				begin
					if (decode_mult)
						mult_enabled = 1;
					mem_read = decode_mem_read_a | decode_mem_read_b;
				end
			`ST_MEM:
				begin
					mem_write = decode_mem_write_a | decode_mem_write_b;
					if (decode_mem_write_a | decode_mem_write_b)
						data_o = TT;
				end
			`ST_WBACK:
				begin
					mem_read = 1;
				end
		endcase
	end

/* Decode
 */

/* synchronous decoder */
reg d_decode_write_reg, d_decode_read_reg, d_decode_jsr, d_decode_jmpz, d_decode_jmp;
reg d_decode_rts,d_decode_alu , d_decode_imm,d_decode_inc_addr_a , d_decode_inc_addr_b, d_decode_dec_addr_a;
reg d_decode_dec_addr_b, d_decode_mem_write_a , d_decode_mem_read_a, d_decode_mem_write_b;
reg d_decode_mem_read_b, d_decode_pushrs, d_decode_poprs, d_decode_pushds, d_decode_popds, d_decode_push_dsp;
reg d_decode_push_rsp, d_decode_swap, d_decode_over, d_decode_roll, d_decode_mult, d_decode_save_top;
reg d_decode_rfd;
always @(data_i)
	begin
		d_decode_write_reg = 0;
		d_decode_read_reg = 0;
		d_decode_jsr = 0;
		d_decode_jmpz = 0;
		d_decode_jmp = 0;
		d_decode_rts = 0;
		d_decode_alu = 0;
		d_decode_imm = 0;
		d_decode_inc_addr_a = 0;
		d_decode_inc_addr_b = 0;
		d_decode_dec_addr_a = 0;
		d_decode_dec_addr_b = 0;
		d_decode_mem_write_a = 0;
		d_decode_mem_read_a = 0;
		d_decode_mem_write_b = 0;
		d_decode_mem_read_b = 0;
		d_decode_pushrs = 0;
		d_decode_poprs = 0;
		d_decode_pushds = 0;
		d_decode_popds = 0;
		d_decode_push_dsp = 0;
		d_decode_push_rsp = 0;
		d_decode_swap = 0;
		d_decode_over = 0;
		d_decode_roll = 0;
		d_decode_mult = 0;
		d_decode_save_top = 0;
		casex(data_i)
			16'b0xxxxxxxxxxxxxxx: begin d_decode_imm = 1; d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx00xxxx: begin d_decode_alu = 1; d_decode_save_top = 1; end // one operand, overwrite
			16'b100xxxxxxx01xxxx: begin d_decode_alu = 1; d_decode_popds = 1; d_decode_save_top = 1; end // two operands, drop one
			16'b100xxxxxxx100000: begin d_decode_pushrs = 1; d_decode_popds = 1; end // >r
			16'b100xxxxxxx100001: begin d_decode_poprs = 1; d_decode_pushds = 1; d_decode_save_top = 1; end // r>
			16'b100xxxxxxx100010: begin d_decode_pushds = 1; end // dup
			16'b100xxxxxxx100011: begin d_decode_popds = 1; end // drop
			16'b100xxxxxxx100100: begin d_decode_swap = 1; end // swap
			16'b100xxxxxxx100101: begin d_decode_over = 1; end // over
			16'b100xxxxxxx100110: begin d_decode_roll = 1; end // roll
			16'b100xxxxxxx100111: begin d_decode_rfd = 1; end // return from debug

			16'b100xxxxxxx101110: begin d_decode_pushds = 1; d_decode_push_dsp = 1; d_decode_save_top = 1; end // push size of data stack
			16'b100xxxxxxx101111: begin d_decode_pushds = 1; d_decode_push_rsp = 1; d_decode_save_top = 1; end // push size of return stack

			16'b100xxxxxxx110000: begin d_decode_mem_write_a = 1; d_decode_popds = 1; end
			16'b100xxxxxxx110001: begin d_decode_mem_write_a = 1; d_decode_inc_addr_a = 1; d_decode_popds = 1; end
			16'b100xxxxxxx110010: begin d_decode_mem_read_a = 1;  d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx110011: begin d_decode_mem_read_a = 1; d_decode_inc_addr_a = 1;  d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx110100: begin d_decode_mem_write_b = 1; d_decode_popds = 1; end
			16'b100xxxxxxx110101: begin d_decode_mem_write_b = 1; d_decode_inc_addr_b = 1; d_decode_popds = 1; end
			16'b100xxxxxxx110110: begin d_decode_mem_read_b = 1; d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx110111: begin d_decode_mem_read_b = 1; d_decode_inc_addr_b = 1;  d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx111000: begin d_decode_mem_write_a = 1; d_decode_dec_addr_a = 1; d_decode_popds = 1; end
			16'b100xxxxxxx111001: begin d_decode_mem_read_a = 1; d_decode_dec_addr_a = 1;  d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx111010: begin d_decode_mem_write_b = 1; d_decode_dec_addr_b = 1; d_decode_popds = 1; end
			16'b100xxxxxxx111011: begin d_decode_mem_read_b = 1; d_decode_dec_addr_b = 1;  d_decode_pushds = 1; d_decode_save_top = 1; end
			16'b100xxxxxxx111100: begin d_decode_mult = 1; d_decode_save_top = 1; end		
			16'b100xxxxxxx111101: begin d_decode_write_reg = 1; d_decode_popds = 1; end // dstack to reg
			16'b100xxxxxxx111110: begin d_decode_pushds = 1; d_decode_read_reg = 1; d_decode_save_top = 1; end // reg to dstack
			16'b100xxxxxxx111111: begin d_decode_rts = 1; end
			
			16'b101xxxxxxxxxxxxx: begin d_decode_jmpz = 1; d_decode_popds = 1; end
			16'b110xxxxxxxxxxxxx: begin d_decode_jsr = 1; end
			16'b111xxxxxxxxxxxxx: d_decode_jmp = 1;
		endcase
	end
	
	
always @(data_i)
	begin
		early_decode_dec_addr_a = 0;
		early_decode_dec_addr_b = 0;
		early_decode_mem_read_a = 0;
		early_decode_mem_read_b = 0;
		casex(data_i)
			16'b100xxxxxxx110010: begin early_decode_mem_read_a = 1; end
			16'b100xxxxxxx110011: begin early_decode_mem_read_a = 1; end
			16'b100xxxxxxx110110: begin early_decode_mem_read_b = 1; end
			16'b100xxxxxxx110111: begin early_decode_mem_read_b = 1; end
			16'b100xxxxxxx111001: begin early_decode_mem_read_a = 1; early_decode_dec_addr_a = 1; end
			16'b100xxxxxxx111011: begin early_decode_mem_read_b = 1; early_decode_dec_addr_b = 1; end
		endcase
	end

	
always @(posedge sys_clk)
	begin
		if (state == `ST_FETCH)
			begin
				decode_read_reg <= d_decode_read_reg;
				decode_write_reg <= d_decode_write_reg;
				decode_jsr <= d_decode_jsr;
				decode_jmpz <= d_decode_jmpz;
				decode_jmp <= d_decode_jmp;
				decode_rts <= d_decode_rts;
				decode_alu <= d_decode_alu;
				decode_imm <= d_decode_imm;
				decode_inc_addr_a <= d_decode_inc_addr_a;
				decode_inc_addr_b <= d_decode_inc_addr_b;
				decode_dec_addr_a <= d_decode_dec_addr_a;
				decode_dec_addr_b <= d_decode_dec_addr_b;
				decode_mem_write_a <= d_decode_mem_write_a;
				decode_mem_read_a <= d_decode_mem_read_a;
				decode_mem_write_b <= d_decode_mem_write_b;
				decode_mem_read_b <= d_decode_mem_read_b;
				decode_pushrs <= d_decode_pushrs;
				decode_poprs <= d_decode_poprs;
				decode_pushds <= d_decode_pushds;
				decode_popds <= d_decode_popds;
				decode_push_dsp <= d_decode_push_dsp;
				decode_push_rsp <= d_decode_push_rsp;
				decode_swap <= d_decode_swap;
				decode_over <= d_decode_over;
				decode_roll <= d_decode_roll;
				decode_mult <= d_decode_mult;
				decode_save_top = d_decode_save_top; 
				decode_rfd <= d_decode_rfd;
			end
	end

initial
	begin
		regpc = 0;
		state = 0;
`ifdef HWDEBUG
		dbg_se_cnt = 0;
		dbg_fe_cnt = 0;
`endif
	end
	
endmodule


module return_stack(
	input wire clk,
	input wire [2:0] state, // sync reset
	input wire push,  // this signal can be active together with save_xx
	input wire pop,   // this signal can be active together with save_xx
	input wire save_top,
	input wire [15:0] to_top, // data to be saved on the top of the stack
	output reg [15:0] SS, // top of the stack
	output wire [3:0] retsp // stack pointer
	);

reg [15:0] rstack[15:0];
reg [3:0] rsp;
assign retsp = rsp;

always @(posedge clk)
	begin
		case (state)
			`ST_PREFETCH:
				rsp <= 4'h0;
			`ST_FETCH:
				begin
					SS <= rstack[0];
				end
			`ST_MEM:
				begin
					if (push) // make room on stack
						begin
							rsp <= rsp + 4'h1; // make room on stack
							rstack[15] <= rstack[14]; rstack[14] <= rstack[13]; rstack[13] <= rstack[12]; rstack[12] <= rstack[11];
							rstack[11] <= rstack[10]; rstack[10] <= rstack[9];  rstack[9]  <= rstack[8];  rstack[8]  <= rstack[7];
							rstack[7]  <= rstack[6];  rstack[6]  <= rstack[5];  rstack[5]  <= rstack[4];  rstack[4]  <= rstack[3];
							rstack[3]  <= rstack[2];  rstack[2]  <= rstack[1];  rstack[1]  <= rstack[0];
						end
					if (pop)
						begin
							rsp <= rsp - 4'h1; // drop from return stack
							rstack[14] <= rstack[15]; rstack[13] <= rstack[14]; rstack[12] <= rstack[13]; rstack[11] <= rstack[12];
							rstack[10] <= rstack[11]; rstack[9]  <= rstack[10]; rstack[8]  <= rstack[9];  rstack[7]  <= rstack[8];
							rstack[6]  <= rstack[7];  rstack[5]  <= rstack[6];  rstack[4]  <= rstack[5];  rstack[3]  <= rstack[4];
							rstack[2]  <= rstack[3];  rstack[1]  <= rstack[2];  rstack[0]  <= rstack[1];
						end
					if (save_top)
							rstack[0] <= to_top;
				end
			
		endcase
	end
		
endmodule
/* Simple data stack
 *
 * The stack operations are valid on the rising edge of the clock
 *
 */
module data_stack(
	input wire clk,
	input wire [2:0] state, // sync reset
	input wire push,  // this signal can be active together with save_xx
	input wire pop,   // this signal can be active together with save_xx
	input wire swap,
	input wire roll,
	input wire over,
	input wire save_dsp,
	input wire save_top,
	input wire save_next,
	input wire [15:0] to_top, // data to be saved on the top of the stack
	input wire [15:0] to_next, // data to be saved on the next of the stack
	output reg [15:0] NN, // next
	output reg [15:0] TT, // top of the stack
	output reg [4:0] datasp // stack pointer
	);


reg [15:0] dstack[31:0];

always @(posedge clk)
	begin
		case (state)
			`ST_PREFETCH:
				datasp <= 5'h0;
			`ST_FETCH:
				begin
					TT <= dstack[0];
					NN <= dstack[1];
				end
			`ST_MEM:
				begin
					if (push | over) // make room on stack
						begin
							datasp <= datasp + 4'h1;
							dstack[31] <= dstack[30]; dstack[30] <= dstack[29];	dstack[29] <= dstack[28]; dstack[28] <= dstack[27];
							dstack[27] <= dstack[26]; dstack[26] <= dstack[25];	dstack[25] <= dstack[24]; dstack[24] <= dstack[23];
							dstack[23] <= dstack[22]; dstack[22] <= dstack[21];	dstack[21] <= dstack[20]; dstack[20] <= dstack[19];
							dstack[19] <= dstack[18]; dstack[18] <= dstack[17];	dstack[17] <= dstack[16]; dstack[16] <= dstack[15];	
							dstack[15] <= dstack[14]; dstack[14] <= dstack[13];	dstack[13] <= dstack[12]; dstack[12] <= dstack[11];
							dstack[11] <= dstack[10]; dstack[10] <= dstack[9]; 	dstack[9]  <= dstack[8];  dstack[8]  <= dstack[7];
							dstack[7]  <= dstack[6];  dstack[6]  <= dstack[5];  dstack[5]  <= dstack[4];  dstack[4]  <= dstack[3];
							dstack[3]  <= dstack[2];  dstack[2]  <= dstack[1]; 
							dstack[1]  <= dstack[0];
						end
					if (pop) // drops a number from the stack
						begin
							datasp <= datasp - 4'h1;
							dstack[30] <= dstack[31]; dstack[29] <= dstack[30];	dstack[28] <= dstack[29]; dstack[27] <= dstack[28];
							dstack[26] <= dstack[27]; dstack[25] <= dstack[26];	dstack[24] <= dstack[25]; dstack[23] <= dstack[24];
							dstack[22] <= dstack[23]; dstack[21] <= dstack[22];	dstack[20] <= dstack[21]; dstack[19] <= dstack[20];
							dstack[18] <= dstack[19]; dstack[17] <= dstack[18];	dstack[16] <= dstack[17]; dstack[15] <= dstack[16];
							dstack[14] <= dstack[15]; dstack[13] <= dstack[14];	dstack[12] <= dstack[13]; dstack[11] <= dstack[12];
							dstack[10] <= dstack[11]; dstack[9]  <= dstack[10]; dstack[8]  <= dstack[9];  dstack[7] <= dstack[8];
							dstack[6]  <= dstack[7];  dstack[5]  <= dstack[6];  dstack[4]  <= dstack[5];  dstack[3]  <= dstack[4];
							dstack[2]  <= dstack[3];  dstack[1]  <= dstack[2];  dstack[0]  <= dstack[1];				
						end
			//	end
			//`ST_WBACK:
			//	begin
					if (save_next) // separated due to data-mux
						dstack[1] <= to_next;
					if (save_top)
						dstack[0] <= to_top;
					if (swap)
						begin
							dstack[1] <= dstack[0];
							dstack[0] <= dstack[1];
						end
					if (roll)
						begin
							dstack[2] <= dstack[1];
							dstack[1] <= dstack[0];
							dstack[0] <= dstack[2];
						end
					if (over)
						dstack[0] <= dstack[1];
					if (save_dsp)
						dstack[0] <= datasp;
				end
		endcase
	end
endmodule

module alu(
	input wire clk,
	input wire [5:0] opcode,
	input wire [6:0] ii, // immediate
	input wire [15:0] NN,
	input wire [15:0] TT,
	input wire regc,
	output reg [15:0] alu_out_o,
	output reg alu_carry_o
	);
	
reg [15:0] alu_out;
reg alu_carry;

// two operand
`define ALU_ADD 	5'b10000
`define ALU_SUB 	5'b10001
`define ALU_AND 	5'b10010
`define ALU_OR  	5'b10011
`define ALU_XOR 	5'b10100
`define ALU_EQ  	5'b10101
`define ALU_NEQ 	5'b10110
`define ALU_GT      5'b10111
`define ALU_LT      5'b11000

// one operand
`define ALU_CMPZ    5'b00000
`define ALU_CMPNZ   5'b00001
`define ALU_PLUSN 	5'b00010
`define ALU_MINUSN 	5'b00011
`define ALU_NOT  	5'b01000
`define ALU_ROL     5'b01100
`define ALU_ROR     5'b01101
`define ALU_SHL 	5'b01110
`define ALU_ASR 	5'b01111
/*
wire [15:0] asr_oo;
wire asr_c;
barrel_asr asr(TT, ii[3:0], asr_c, asr_oo);
*/
/* ALU */
always @(opcode, ii, NN, TT, regc)//, asr_c, asr_oo)
	begin
		alu_carry = 1'b0;
		alu_out = 16'h0;
		case (opcode[5:0])
			`ALU_ADD: { alu_carry, alu_out } = { 1'b0, NN } + { 1'b0, TT };
			`ALU_SUB: { alu_carry, alu_out } = { 1'b0, NN } - { 1'b0, TT };
			`ALU_CMPZ: alu_out = (TT == 16'h0000) ? 16'hffff:16'h0000;
			`ALU_CMPNZ: alu_out = (TT != 16'h0000) ? 16'hffff:16'h0000;
			`ALU_PLUSN:{ alu_carry, alu_out } = { 1'b0, TT } + { 10'h0, ii};
			`ALU_MINUSN:{ alu_carry, alu_out } = { 1'b0, TT } - { 10'h0, ii };
			`ALU_AND: alu_out = NN & TT;
			`ALU_OR:  alu_out = NN | TT;
			`ALU_XOR: alu_out = NN ^ TT;
			`ALU_EQ:  alu_out = (NN == TT) ? 16'hffff:16'h0000;
			`ALU_NEQ:  alu_out = (NN != TT) ? 16'hffff:16'h0000;
			`ALU_GT:{ alu_carry, alu_out } = (NN > TT) ? 17'h1ffff:17'h00000;
			`ALU_LT:{ alu_carry, alu_out } = (NN < TT) ? 17'h1ffff:17'h00000;
			`ALU_NOT:  alu_out = ~TT;
			`ALU_ROL: { alu_carry, alu_out } = { TT, regc };
			`ALU_ROR: { alu_out, alu_carry } = { regc, TT };
			`ALU_SHL: { alu_carry, alu_out } = { TT, 1'b0 };
			`ALU_ASR: { alu_out, alu_carry } = { TT[15], TT };//{ asr_oo, asr_c };
		endcase
	end

always @(posedge clk)
	begin
		alu_out_o <= alu_out;
		alu_carry_o <= alu_carry_o;
	end

endmodule
/*
module barrel_asr(
	input wire [15:0] TT,
	input wire [3:0] shift,
	output wire carry,
	output wire [15:0] oo);

wire [16:0] bit0o, bit1o, bit2o;

assign bit0o = shift[0] ? { TT[15], TT }:{ TT, 1'b0 };
assign bit1o = shift[1] ? { bit0o[15], bit0o[15], bit0o[15:2] }:{ bit0o, 1'b0 };
assign bit2o = shift[2] ? { bit1o[15], bit1o[15], bit1o[15], bit1o[15], bit1o[15:4] }:{ bit1o, 1'b0 };
assign { oo, carry } = shift[3] ? { bit2o[15], bit2o[15], bit2o[15], bit2o[15], bit2o[15], bit2o[15], bit2o[15], bit2o[15], bit2o[15:8] }:{ bit2o, 1'b0 };

endmodule

module barrel_asr(
	input wire [15:0] TT,
	input wire [3:0] shift,
	output reg carry,
	output reg [15:0] oo);
	
always @(TT, shift)
	case (shift)
		4'h0: { oo, carry } = { TT, 1'b0 };
		4'h1: { oo, carry } = { TT[15], TT };
		4'h2: { oo, carry } = { TT[15], TT[15], TT[15:1] };
		4'h3: { oo, carry } = { TT[15], TT[15], TT[15], TT[15:2] };
		4'h4: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15:3] };
		4'h5: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:4] };
		4'h6: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:5] };
		4'h7: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:6] };
		4'h8: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:7] };
		4'h9: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:8] };
		4'ha: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:9] };
		4'hb: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:10] };
		4'hc: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:11] };
		4'hd: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:12] };
		4'he: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:13] };
		4'hf: { oo, carry } = { TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15], TT[15:14] };
	endcase
	
endmodule
*/

/* Booth's multiplication algorithm */
/* multiplies 16x16 in 16 clocks */
module boothmult(
	input wire clk_i,
	input wire en_i,
	input wire [15:0] m_i,
	input wire [15:0] r_i,
	output wire [31:0] r_o,
	output wire mult_ready
	);
parameter IBITS = 32;
`define PBITS 16
`define PBITSM1 15

reg [IBITS:0] A, S, P;
reg [3:0] cnt;
wire [IBITS:0] PPA, PSA;
wire [15:0] neg_m;

assign PPA = P + A;
assign PSA = P + S;
assign r_o = P[IBITS:1];
assign neg_m = `PBITS'h0-m_i[`PBITSM1:0];
assign mult_ready = cnt != 4'h0;

always @(posedge clk_i)
	begin
		if (en_i == 1'b1)
			begin
				A <= { m_i, `PBITS'h0, 1'b0 };
				S <= { neg_m, `PBITS'h0, 1'b0 };
				//P <= { `PBITS'h0, r_i[`PBITSM1:0], 1'h0 };
				if (r_i[0] == 1'b0) // just shift
					P <= { `PBITS'h0, 1'h0, r_i[`PBITSM1:0] };
				else
					P <= { neg_m[15], neg_m, r_i[`PBITSM1:0] };
				cnt <= 4'd15;
			end
		else
			if (cnt > 0)
				begin
					case (P[1:0])
						2'b00: P <= { P[IBITS], P[IBITS:1] };
						2'b01: P <= { PPA[IBITS], PPA[IBITS:1] };
						2'b10: P <= { PSA[IBITS], PSA[IBITS:1] };
						2'b11: P <= { P[IBITS], P[IBITS:1] };
					endcase
					$display("%08x %08x %09x %09x = %09x %08x %2d", m_i, r_i, A, S, P, P[IBITS:1], cnt);
					cnt <= cnt - 4'd1;
				end
	end
	
initial
	cnt = 0;
	
endmodule


// uart.v
// simple low speed async uart for RS-232
// brad@heeltoe.com 2009

module uart(clk, reset,
	    txclk, ld_tx_req, ld_tx_ack, tx_data, tx_enable, tx_out, tx_empty,
	    rxclk, uld_rx_req, uld_rx_ack, rx_data, rx_enable, rx_in,rx_empty);
   
   input   wire     clk;
   input   wire     reset;
   input   wire     txclk;
   input   wire     ld_tx_req;
   output  reg      ld_tx_ack;
   input   wire[7:0]	tx_data;
   input   wire     tx_enable;
   output  reg     tx_out;
   output  reg     tx_empty;
   input   wire     rxclk;
   input   wire     uld_rx_req;
   output reg       uld_rx_ack;
   output reg [7:0] rx_data;
   input   wire     rx_enable;
   input   wire     rx_in;
   output  reg     rx_empty;

   //reg 		ld_tx_ack;
   //reg 		uld_rx_ack;
 		
   reg [7:0] 	tx_reg;
   //reg          tx_empty;
   reg          tx_over_run;
   reg [3:0] 	tx_cnt;
   //reg          tx_out;
   reg [7:0] 	rx_reg;
   //reg [7:0] 	rx_data;
   reg [3:0] 	rx_sample_cnt;
   reg [3:0] 	rx_cnt;  
   reg          rx_frame_err;
   reg          rx_over_run;
   //reg          rx_empty;
   reg          rx_d1;
   reg          rx_d2;
   reg          rx_busy;

   reg [1:0]	rx_uld;
   reg [1:0] 	rx_uld_next;
   
   reg [1:0]	tx_ld;
   reg [1:0] 	tx_ld_next;

   // require uld_rx_req to deassert before sending next char
   always @(posedge rxclk or posedge reset)
     if (reset)
       rx_uld <= 2'b00;
     else
       rx_uld <= rx_uld_next;

   always @(uld_rx_req or rx_uld)
     begin
	rx_uld_next = rx_uld;
	uld_rx_ack <= 0;
	case (rx_uld)
	  2'b00: if (uld_rx_req) rx_uld_next = 2'b01;
	  2'b01: begin
	     uld_rx_ack <= 1;
	     rx_uld_next = 2'b10;
	    end
	  2'b10: begin
	     uld_rx_ack <= 1;
	     if (~uld_rx_req) rx_uld_next = 2'b00;
	    end
	  default: rx_uld_next = 2'b00;
	endcase
     end

   wire uld_rx_data;
   assign uld_rx_data = rx_uld == 2'b01;
   
   // require tx_ld_req to deassert before accepting next char
   always @(posedge txclk or posedge reset)
     if (reset)
       tx_ld <= 2'b00;
     else
       tx_ld <= tx_ld_next;

   always @(ld_tx_req or tx_ld)
     begin
	tx_ld_next = tx_ld;
	ld_tx_ack <= 0;
	case (tx_ld)
	  2'b00: if (ld_tx_req) tx_ld_next = 2'b01;
	  2'b01: begin
	     ld_tx_ack <= 1;
	     tx_ld_next = 2'b10;
	    end
	  2'b10: begin
	     ld_tx_ack <= 1;
	     if (~ld_tx_req) tx_ld_next = 2'b00;
	    end
	  default: tx_ld_next = 2'b00;
	endcase
     end
   
   wire ld_tx_data;
   assign ld_tx_data = tx_ld == 2'b01;
   
   
   // uart rx
   always @(posedge rxclk or posedge reset)
     if (reset)
       begin
	  rx_reg <= 0; 
	  rx_data <= 0;
	  rx_sample_cnt <= 0;
	  rx_cnt <= 0;
	  rx_frame_err <= 0;
	  rx_over_run <= 0;
	  rx_empty <= 1;
	  rx_d1 <= 1;
	  rx_d2 <= 1;
	  rx_busy <= 0;
       end
     else
       begin
	  // synchronize the asynch signal
	  rx_d1 <= rx_in;
	  rx_d2 <= rx_d1;

	  // uload the rx data
	  if (uld_rx_data && ~rx_empty)
	    begin
	       rx_data <= rx_reg;
	       rx_empty <= 1;
	  end

	  // receive data only when rx is enabled
	  if (rx_enable)
	    begin
	       // check if just received start of frame
	       if (!rx_busy && !rx_d2)
		 begin
		    rx_busy <= 1;
		    rx_sample_cnt <= 1;
		    rx_cnt <= 0;
		 end
	       
	       // start of frame detected
	       if (rx_busy)
		 begin
		    rx_sample_cnt <= rx_sample_cnt + 4'd1;
		    
		    // sample at middle of data
		    if (rx_sample_cnt == 7)
		      begin
			 if ((rx_d2 == 1) && (rx_cnt == 0))
			   rx_busy <= 0;
			 else
			   begin
			      rx_cnt <= rx_cnt + 4'd1; 

			      // start storing the rx data
			      if (rx_cnt > 0 && rx_cnt < 9)
				   rx_reg[rx_cnt - 1] <= rx_d2;

			      if (rx_cnt == 4'd9)
				begin
				   //$display("rx_cnt %d, rx_reg %o",
				   //  rx_cnt, rx_reg);
				   
				   rx_busy <= 0;

				   // check if end of frame received correctly
				   if (rx_d2 == 0)
				     rx_frame_err <= 1;
				   else
				     begin
					rx_empty <= 0;
					rx_frame_err <= 0;

					// check for overrun
					rx_over_run <= (rx_empty) ?
							 1'b0 : 1'b1;
				     end
				end
			   end
		      end 
		 end 
	    end

	  if (!rx_enable)
	    rx_busy <= 0;
       end

    // uart tx
    always @ (posedge txclk or posedge reset)
      if (reset)
	begin
	   tx_empty <= 1'b1;
	   tx_out <= 1'b1;
	   tx_cnt <= 4'b0;

	   tx_reg <= 0;
	   tx_over_run <= 0;
	end
      else
	begin
   	   if (ld_tx_data)
	     begin
		if (!tx_empty)
		  tx_over_run <= 1;
		else
		  begin
		     tx_reg <= tx_data;
		     tx_empty <= 0;
		  end
	     end

	  if (tx_enable && !tx_empty)
	    begin
	       tx_cnt <= tx_cnt + 1'b1;

	       case (tx_cnt)
		 4'd0: tx_out <= 0;
		 4'd1: tx_out <= tx_reg[0];
		 4'd2: tx_out <= tx_reg[1];
		 4'd3: tx_out <= tx_reg[2];
		 4'd4: tx_out <= tx_reg[3];
		 4'd5: tx_out <= tx_reg[4];
		 4'd6: tx_out <= tx_reg[5];
		 4'd7: tx_out <= tx_reg[6];
		 4'd8: tx_out <= tx_reg[7];
		 4'd9: begin
		    tx_out <= 1;
		    tx_cnt <= 0;
		    tx_empty <= 1;
		 end
	       endcase
	    end

	  if (!tx_enable)
	    tx_cnt <= 0;
	end
   
endmodule

// brg.v
// baud rate generator for uart

module brg(clk, reset, tx_baud_clk, rx_baud_clk);

   input wire clk;
   input wire reset;
   output reg tx_baud_clk;
   output reg rx_baud_clk;

   parameter SYS_CLK = 40_000_000; //50000000;
   parameter BAUD = 38400;

`ifdef sim_time
   parameter RX_CLK_DIV = 2;
   parameter TX_CLK_DIV = 2;
`else
   parameter RX_CLK_DIV = SYS_CLK / (BAUD * 16 * 2);
   parameter TX_CLK_DIV = SYS_CLK / (BAUD * 2);
`endif
   
   reg [12:0] rx_clk_div;
   reg [12:0] tx_clk_div;

   always @(posedge clk or posedge reset)
     if (reset)
       begin
	  rx_clk_div  <= 0;
	  rx_baud_clk <= 0; 
       end
     else 
       if (rx_clk_div == RX_CLK_DIV)
	 begin
	    rx_clk_div  <= 0;
	    rx_baud_clk <= ~rx_baud_clk;
	 end
       else
	 begin
	    rx_clk_div  <= rx_clk_div + 1'b1;
	    rx_baud_clk <= rx_baud_clk;
	 end

   always @(posedge clk or posedge reset)
     if (reset)
       begin
	  tx_clk_div  <= 0;
	  tx_baud_clk <= 0; 
       end
     else 
       if (tx_clk_div == TX_CLK_DIV)
	 begin
	    tx_clk_div  <= 0;
	    tx_baud_clk <= ~tx_baud_clk;
	 end
       else
	 begin
	    tx_clk_div  <= tx_clk_div + 1'b1;
	    tx_baud_clk <= tx_baud_clk;
	 end
   
endmodule

/*



Release Notes
12.11.2013 simpleforth_008.v
- HW Debug not yet working but download program does work

03.11.2013
- HW Debug changed with debug modus. 


01.11.2013
- 1+/2+ merged 1-/2- merged
start:
Preference Summary

FREQUENCY NET "clk_c_c" 2.080000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   54.095MHz is the maximum frequency for this preference.

FREQUENCY 40.000000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   51.044MHz is the maximum frequency for this preference.

Design Summary
   Number of registers:    1760
      PFU registers:    1757
      PIO registers:    3
   Number of SLICEs:          1141 out of  3432 (33%)
      SLICEs(logic/ROM):       858 out of   858 (100%)
      SLICEs(logic/ROM/RAM):   283 out of  2574 (11%)
          As RAM:            0 out of  2574 (0%)
          As Logic/ROM:    283 out of  2574 (11%)
   Number of logic LUT4s:     1808
   Number of distributed RAM:   0 (0 LUT4s)
   Number of ripple logic:    228 (456 LUT4s)
   Number of shift registers:   0
   Total number of LUT4s:     2264
   Number of PIO sites used: 57 out of 115 (50%)
   Number of block RAMs:  16 out of 26 (62%)


after merge:
Preference Summary

FREQUENCY NET "clk_c_c" 2.080000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   54.813MHz is the maximum frequency for this preference.

FREQUENCY 40.000000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   55.522MHz is the maximum frequency for this preference.


28.10.2013 simpleforth_006.v
- Stack moved to its own module
- 


27.10.2013 simpleforth_005.v
- changed negedge for posedge for the change of state, frequency rised to 54 Mhz
- debug is flaky, eats received chars from time to time


Preference Summary

FREQUENCY NET "clk_c_c" 2.080000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   54.478MHz is the maximum frequency for this preference. (53 with data bus to pins and ALU as module)

FREQUENCY 40.000000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   51.720MHz is the maximum frequency for this preference.


23.10.13 simpleforth_004.v
- Changed address MUX
- Added a pre-mux and register to simplify the data path

Preference Summary

FREQUENCY NET "clk_c_c" 2.080000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   32.731MHz is the maximum frequency for this preference.

FREQUENCY 40.000000 MHz (0 errors)
            4096 items scored, 0 timing errors detected.
Report:   56.680MHz is the maximum frequency for this preference.


22.10.13
- Changed NN, TT and SS to be registers loaded at FETCH time. This increased
  the max speed again to 32 MHz from 23 because of the multiplier
  The critical path:
       Data path cpu/SLICE_861 to cpu/SLICE_337:

   Name    Fanout   Delay (ns)          Site               Resource
REG_DEL     ---     0.452    R19C22D.CLK to     R19C22D.Q1 cpu/SLICE_861 (from clk_c_c)
ROUTE       530     4.884     R19C22D.Q1 to     R17C22D.D1 state_o_c[1]
CTOF_DEL    ---     0.495     R17C22D.D1 to     R17C22D.F1 cpu/SLICE_1074
ROUTE        42     1.552     R17C22D.F1 to     R16C19A.A0 cpu/dstack_1__2_sqmuxa_sn
CTOF_DEL    ---     0.495     R16C19A.A0 to     R16C19A.F0 cpu/SLICE_1075
ROUTE         4     1.110     R16C19A.F0 to     R15C20B.C1 cpu/dstack_0__5_sqmuxa_3
CTOF_DEL    ---     0.495     R15C20B.C1 to     R15C20B.F1 cpu/mult/SLICE_1086
ROUTE        16     1.792     R15C20B.F1 to     R16C22B.A0 cpu/mult/dstack_0__3_sqmuxa
CTOF_DEL    ---     0.495     R16C22B.A0 to     R16C22B.F0 cpu/mult/SLICE_1103
ROUTE         1     2.839     R16C22B.F0 to     R15C19D.D1 cpu/mult/dstack_0__22_iv_4[1]
CTOF_DEL    ---     0.495     R15C19D.D1 to     R15C19D.F1 cpu/SLICE_337
ROUTE         1     0.000     R15C19D.F1 to    R15C19D.DI1 cpu/dstack_0__22[1] (to clk_c_c)
                  --------
                   15.104   (19.4% logic, 80.6% route), 6 logic levels.

 The access to the dstack should be cleaner

 1105 SLICEs
 32.744 MHz for clk_c_
 55.015 MHz for 40 MHz net


*/