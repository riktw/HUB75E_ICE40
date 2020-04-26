module internalClock
(
    output  wire        io_intclk, 
);
SB_HFOSC #( .CLKHF_DIV ("0b01") ) // 48MHz DIVIDED by 2
myOSC(
    .CLKHFEN(1'b1),
    .CLKHFPU(1'b1),
    .CLKHF(io_intclk)
);
endmodule
