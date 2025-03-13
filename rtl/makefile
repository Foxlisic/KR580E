all:
	iverilog -g2005-sv -DICARUS=1 -o main.qqq tb.v ../kr580.v
	vvp main.qqq >> /dev/null
vcd:
	gtkwave tb.vcd
wav:
	gtkwave tb.gtkw
