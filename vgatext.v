
module vgatext(
	input wire CLK,
	input wire RESET,
	output wire HSYNC,
	output wire VSYNC,
	output wire RED,
	output wire GREEN,
	output wire BLUE,
	input wire WrClk,
	input wire [11:0] WrAddr,
	input wire WrEn,
	input wire [7:0] WrData
	);
`ifdef CLK25
`define LINE_LENGTH    11'd831
`define HSYNC_START    11'd704+11'd64+11'd32
`define HVISIBLE_START 11'd16+11'd64
`define HVISIBLE_END   11'd656+11'd64
`define VVISIBLE_START 11'd10
`define VVISIBLE_END   11'd490
`define FRAME_END      11'd525
`define VSYNC_START    11'd524
`else
/* For 40 MHz clk 800x600 60 Hz */
`define LINE_LENGTH    11'd1055
`define HSYNC_START    11'd840+11'd88
`define HVISIBLE_START 11'd40
`define HVISIBLE_END   11'd840
`define VVISIBLE_START 11'd16
`define VVISIBLE_END   11'd616
`define FRAME_END      11'd627
`define VSYNC_START    11'd624
`endif
reg [10:0] hsync_cnt, vsync_cnt;
reg [3:0] redr, greenr, bluer;
reg hsyncr, vsyncr;

assign HSYNC = hsyncr;//hsync_cnt >= 11'd1408 ? 0:1; // negative pulse
assign VSYNC = vsyncr;//vsync_cnt >= 11'd1048 ? 0:1; // negative pulse

reg visible;

//RAMB16_S9_S9 buff(DOA, DOPA, DOB, DOPB, ADDRA, CLKA, DIA, DIPA, ENA, SSRA, WEA, ADDRB, CLKB, DIB, DIPB, ENB, SSRB, WEB);
//RAMB16_S9_S9 font(DOA, DOPA, DOB, DOPB, ADDRA, CLKA, DIA, DIPA, ENA, SSRA, WEA, ADDRB, CLKB, DIB, DIPB, ENB, SSRB, WEB);

assign RED = visible ? redr[0]:0;
assign GREEN = visible ? greenr[0]:0;
assign BLUE = visible ? bluer[0]:0;
 
always @(posedge CLK)
	begin
		if (RESET == 1'b0)
			begin
				hsync_cnt <= 11'h0;
				vsync_cnt <= 11'h0;
			end
		else
			begin
				if (hsync_cnt == `LINE_LENGTH) // end of line
					begin
						hsync_cnt <= 0;
						hsyncr <= 1'b1;
						if (vsync_cnt == `FRAME_END)
							vsync_cnt <= 11'd0;
						else
							vsync_cnt <= vsync_cnt + 11'd1;
					end
				else
					hsync_cnt <= hsync_cnt + 11'd1;
			end
		hsyncr <= hsync_cnt >= `HSYNC_START ? 0:1;
		vsyncr <= vsync_cnt >= `VSYNC_START ? 0:1;
		visible <= (hsync_cnt >= `HVISIBLE_START) && (hsync_cnt < `HVISIBLE_END) && (vsync_cnt >= `VVISIBLE_START) && (vsync_cnt < `VVISIBLE_END);
		//redr <= hsync_cnt[3:0];
		//greenr <= hsync_cnt[7:4];
		//bluer <= hsync_cnt[9:6];

	end

wire enable = (hsync_cnt >= `HVISIBLE_START-11'd8) && (hsync_cnt < `HVISIBLE_END) && (vsync_cnt >= `VVISIBLE_START) && (vsync_cnt < `VVISIBLE_END);
reg [6:0] x_cnt, y_cnt, cur_x, cur_y;
reg [3:0] line_cnt;
reg [7:0] chars_data, font_data, tshift, shift;
wire [7:0] font_bus, chars_bus;
/*
bramfont font(.clka(CLK), .addra({ chars_data, line_cnt }), .douta(font_bus));
bramchars chars(.clka(CLK), .clkb(CLK), .dina(8'd0),
	.addra(y_cnt[4:0] * 80 + x_cnt),
	.wea(1'b0),
	.douta(chars_bus),
	
	.dinb(8'd0),
	.addrb(12'd0),
	.web(1'b0),
	.doutb());
*/
wire [11:0] yptr;assign yptr = { y_cnt[5:0], 6'h0 } + { y_cnt[5:0], 5'h0 } + { y_cnt[5:0], 2'h0 } + { 4'h0, x_cnt };

fontrom font(.Address({ chars_data, line_cnt }), .OutClock(CLK), .OutClockEn(1'b1), .Reset(1'b0), .Q(font_bus));

textmem4k chars(.WrAddress(WrAddr), .RdAddress(yptr), .Data(WrData), .WE(WrEn), .RdClock(CLK), .RdClockEn(1'b1), 
    .Reset(1'b0), .WrClock(WrClk), .WrClockEn(1'b1), .Q(chars_bus));

always @(posedge CLK) // read memory
	begin
		chars_data <= chars_bus;//[{ y_cnt[4:0], x_cnt }];
		font_data <= font_bus;
	end

always @(posedge CLK)
	begin
		if (hsync_cnt == 0)
			begin
				x_cnt <= 0;
				cur_x <= 3;
			end
		if (vsync_cnt == 0)
			begin
				y_cnt <= 0;
				line_cnt <= 0;
				cur_y <= 1;
			end
		if ((hsync_cnt == `LINE_LENGTH) && (vsync_cnt >= `VVISIBLE_START))
			begin
				$display(" ");
				line_cnt <= line_cnt + 1;
				if (line_cnt == 4'hf)
					y_cnt <= y_cnt + 1;
			end
		if (enable)
			begin
				
				case (hsync_cnt[2:0]) // start of group of 8 consecutive pixels
					0: // read new char/color
						begin    
							 shift <= shift << 1;
						    x_cnt <= x_cnt + 1;
						end
					1: // reads font
						shift <= shift << 1;
					2: // load font data into shift register
						begin
							shift <= shift << 1;
							tshift <= font_data;
						end
					3, 4, 5, 6:
						shift <= shift << 1;
					7: // uses read shift register
						begin
							shift <= tshift;
							
						end
				endcase
				if ((cur_x == x_cnt) && (cur_y == y_cnt) && (line_cnt > 4'd13))
					begin
						redr <= 4'hf;
						greenr <= 4'hf;
						bluer <= 4'hf;
						$write("!");
					end
				else
					begin
						redr <= shift[7] ? 4'hf:4'h0;
						greenr <= shift[7] ? 4'hf:4'h0;
						bluer <= shift[7] ? 4'hf:4'h0;
						$write("%c", shift[7] ? 33:32);
					end
			end
		else
			begin
				redr <= 4'h0;
				greenr <= 4'h0;
				bluer <= 4'h0;			
			end
	end
initial
	begin
		redr = 0;
		greenr = 0;
		bluer = 0;
		vsync_cnt = 0;
		hsync_cnt = 0;
	end
endmodule
