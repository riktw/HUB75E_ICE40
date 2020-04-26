module Ram_1wrs #( 
    parameter wordCount = 3072, 
    parameter wordWidth = 32,
    parameter readUnderWrite = "dontCare",
    parameter technology = "auto",
    parameter maskWidth = 1,
    parameter maskEnable = 1'b0 
  ) 
  ( 
    input clk, //i
    input en, //i
    input wr, //i
    input [11:0] addr, //i
    input mask, //i
    input [31:0] wrData, //i
    output [31:0] rdData
  );
  
  SB_SPRAM256KA spram0
  (
    .ADDRESS(addr),
    .DATAIN(wrData[31:16]),
    .MASKWREN({wr, wr, wr, wr}),
    .WREN(wr),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(1'b0),
    .POWEROFF(1'b1),
    .DATAOUT(rdData[31:16])
  );
  
  SB_SPRAM256KA spram1
  (
    .ADDRESS(addr),
    .DATAIN(wrData[15:0]),
    .MASKWREN({wr, wr, wr, wr}),
    .WREN(wr),
    .CHIPSELECT(1'b1),
    .CLOCK(clk),
    .STANDBY(1'b0),
    .SLEEP(1'b0),
    .POWEROFF(1'b1),
    .DATAOUT(rdData[15:0])
  );
  
endmodule
