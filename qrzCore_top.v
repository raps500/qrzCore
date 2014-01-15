/*
 * Top entity for qrzCore
 * Works on the Lattice MachXO2-7000 breakout board
 */
 
 
module qrzCore_top(
	output wire [7:0] leds,
	output wire clk,
	output wire [15:0] addr,
	output wire vsync_o,
	output wire hsync_o,
	output wire red_o,
	output wire green_o,
	output wire blue_o
	);
	
wire internal_clk, internal_reset;
wire osc_clk, char_clk;
defparam OSCH_inst.NOM_FREQ = "26.60"; //"29.56"; //33.25//"24.18"; // 2.08
OSCH OSCH_inst(.STDBY(1'b0), .OSC(osc_clk), .SEDSTDBY());

reg [2:0] resetcnt;
reg [21:0] divcnt;
assign internal_reset = resetcnt == 3'd6;

always @(posedge osc_clk)
	begin
		divcnt <= divcnt + 22'b1;
	end

assign internal_clk = divcnt[21];
assign char_clk = divcnt[16];
always @(posedge internal_clk)
	begin
		if (resetcnt != 3'd6)
			resetcnt <= resetcnt + 3'd1;
	end

wire oen, cen, wen;
wire [15:0] addr_o, data_to_cpu, data_from_cpu;
wire [2:0] state;
assign clk = internal_clk;
assign addr = addr_o;
assign leds = { state, addr_o[4:0] };

qrzCore cpu(
	.sys_clk(internal_clk),
	.cpu_addr_o(addr_o),
	.cpu_oen_o(oen),
	.cpu_wen_o(wen),
	.cpu_cen_o(cen),
	.cpu_data_i(data_to_cpu),
	.cpu_data_o(data_from_cpu),
	.cpu_state_o(state)
	);


ram4k ram(.WrAddress(addr_o), .RdAddress(addr_o), .Data(data_from_cpu), .WE(!wen), .RdClock(internal_clk), .RdClockEn(!oen), .Reset(1'b0), 
    .WrClock(internal_clk), .WrClockEn(1'b1), .Q(data_to_cpu));

vgatext(
	.CLK(osc_clk),
	.RESET(internal_reset),
	.HSYNC(hsync_o),
	.VSYNC(vsync_o),
	.RED(red_o),
	.GREEN(green_o),
	.BLUE(blue_o)
	);


endmodule
