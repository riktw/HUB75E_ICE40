TARGET_STEM = RgbMatrixControllerSpi

NEXTPNR_DEVICE = up5k
PACKAGE        = sg48
ICETIME_DEVICE = up5k

PROG_BIN       = iceprog

PINS_FILE = ./src/Ice40/pins.pcf

YOSYS_LOG  = synth.log
YOSYS_ARGS = -v3 -l $(YOSYS_LOG)

VERILOG_MAIN_SRC = $(TARGET_STEM).v 
VERILOG_SRCS = src/verilog/single_port_ram.v src/verilog/inthsclk.v

BIN_FILE  = $(TARGET_STEM).bin
ASC_FILE  = $(TARGET_STEM).asc
JSON_FILE = $(TARGET_STEM).json

all:	$(BIN_FILE)

sim:
	sbt "runMain mylib.LEDCounterSim"

$(BIN_FILE):	$(ASC_FILE)
	icepack	$< $@

$(ASC_FILE):	$(JSON_FILE) $(PINS_FILE)
	nextpnr-ice40 --$(NEXTPNR_DEVICE) --package $(PACKAGE) --freq 24 --json $(JSON_FILE) --pcf $(PINS_FILE) --asc $(ASC_FILE)

$(JSON_FILE):	$(VERILOG_MAIN_SRC)
	yosys $(YOSYS_ARGS) -p "synth_ice40 -json $(JSON_FILE)" $(VERILOG_MAIN_SRC) $(VERILOG_SRCS)

$(VERILOG_MAIN_SRC):
	sbt "runMain mylib.MyTopLevelVerilogSpi"
	
prog:	$(BIN_FILE)
	$(PROG_BIN) $<

timings:$(ASC_FILE)
	icetime -tmd $(ICETIME_DEVICE) $<

clean:
	sbt clean
	rm -f $(BIN_FILE) $(ASC_FILE) $(JSON_FILE) $(YOSYS_LOG) $(VERILOG_MAIN_SRC)

.PHONY:	all clean prog timings


