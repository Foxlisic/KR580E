`timescale 10ns / 1ns

/*
 * Эмулятор КР580*** какого-то там компа
 */

module tb;

// ---------------------------------------------------------------------

reg         clk, clock;
always #0.5 clk         = ~clk;
always #2.0 clock       = ~clock;

initial begin pin_intr = 0; clk = 1; clock = 0; #2 pin_intr = 1'b1; #2500 $finish; end
initial begin $dumpfile("tb.vcd"); $dumpvars(0, tb); end

// ---------------------------------------------------------------------

reg  [ 7:0] memory[65536];
reg  [ 7:0] in;
wire [ 7:0] out;
wire [15:0] address;

initial $readmemh("tb.hex", memory, 16'h0000);

/* Формируется логика чтения и записи в память */
always @(posedge clk) begin in <= memory[address]; if (we) memory[address] <= out; end

wire [7:0] pin_pi;
wire [7:0] pin_po;
wire       pin_pw, pin_pr;
reg        pin_intr;

// ---------------------------------------------------------------------

kr580 cpu(

    .clock      (clock),
    .reset_n    (1'b1),
    .locked     (1'b1),
    .in         (in),
    .address    (address),
    .we         (we),
    .out        (out),
    // ---
    .pw         (pin_pw),
    .pr         (pin_pr),
    .intr       (pin_intr)
);

endmodule
