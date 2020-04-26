// Info: https://bikerglen.com/projects/lighting/led-panel-1up/

package mylib

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._

import scala.util.Random

case class RGBBundle() extends Bundle {
    val R = Bool
    val G = Bool
    val B = Bool
}

case class rowBundle() extends Bundle {
     val rowData = Vec(Bits(64 bits), 6)  //R0, R1, G0, G1, B0, B1
}

case class rowNumberBundle() extends Bundle {
    val rowNumber = UInt(5 bits)
    val bcmNumber = UInt(3 bits)
}


/*
The WishboneFrameBuffer contains the dual framebuffer.
From Wishbone, data is transferred to one framebuffer while the other is displayed.
Via a wishbone command the framebuffers are switched. 
Brightness is also set via a wishbone command, and stored in a register in this class.
Framebuffers are stored in single port RAM so it can also work on 
*/
class WishboneFrameBuffer(config : WishboneConfig, debug : Boolean) extends Component {
    val io = new Bundle {
        val wb = slave(Wishbone(config))
        val dataCommand = slave Stream (rowNumberBundle())
        val dataResponse = master Flow (rowBundle())
        val brightness = out UInt(6 bits)
    }
    
    val frameBuffer0 = Mem(Bits(32 bits), wordCount = 128*24)   
    val frameBuffer1 = Mem(Bits(32 bits), wordCount = 128*24)
    if(debug == false){
        frameBuffer0.generateAsBlackBox()
        frameBuffer1.generateAsBlackBox()
    }
    
    val writeToFrameBuffer = io.wb.ADR(14)
    
    val brightnessReg = Reg(UInt(8 bits)) init (0)
    val frameBufferSelected = Reg(Bool) init (False)
    
    val wishboneFactory = WishboneSlaveFactory(io.wb)
    wishboneFactory.driveAndRead(brightnessReg, 0)
    wishboneFactory.onWrite(4)(frameBufferSelected := !frameBufferSelected)
    
    io.brightness := brightnessReg(5 downto 0)

    //-------------------------------------------------------------
    val getDataCounter = Reg(UInt(3 bits)) init(0)
    val concat32To64BitMemoryMux = Reg(UInt(1 bits)) init(0)
    val data = Reg(rowBundle())
    
    val baseCounter = U(0, 11 bits)
    
    val dataFrameBuffer0 = Bits(32 bits)
    val dataFrameBuffer1 = Bits(32 bits)
    val dataMemConcatenated = Reg(Bits(64 bits)) init(0)
    dataFrameBuffer0 := B"x00000000"
    dataFrameBuffer1 := B"x00000000"

    io.dataCommand.ready := False
    io.dataResponse.valid := False
    io.dataResponse.payload := data
    
    //To make do with one single port memory block per framebuffer, a total of 7 * 2 = 14 cycles is needed 
    // to fetch 6*64 bit of RGB data. 
    when(io.dataCommand.valid) {
        baseCounter := ((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24)).resized
        
        when(getDataCounter === 7) {
            io.dataCommand.ready := True;
            io.dataResponse.valid := True
            io.dataResponse.payload := data
        }
        .otherwise {
            // data stored as Red row LSB -> MSB, green row LSB -> MSB, blue row LSB -> MSB. Hence reading with an offset of 8
            // lower 32 rows are stored 3*8*32=768 after the upper rows, hence offset of 768
            when(getDataCounter === 0){
                baseCounter := ((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24)).resized
            }
            .elsewhen(getDataCounter === 1){
                baseCounter := (((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24)) +^ 768)
            }
            .elsewhen(getDataCounter === 2){
                baseCounter := ((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24) + U"11'd8").resized
            }
            .elsewhen(getDataCounter === 3){
                baseCounter := (((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24)) +^ (768 + 8))
            }
            .elsewhen(getDataCounter === 4){
                baseCounter := ((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24) + U"11'd16").resized
            }
            .elsewhen(getDataCounter === 5){
                baseCounter := (((io.dataCommand.payload.bcmNumber) + (io.dataCommand.payload.rowNumber * 24)) +^ (768 + 16))
            }
            
            //Some magic is needed as single port memory is used, muxes select correct read or write address based on 
            //frameBufferSelected and concat32To64BitMemoryMux
            dataFrameBuffer0 := frameBuffer0.readWriteSync(
                enable  = True,
                address = Mux(frameBufferSelected, io.wb.ADR(11 downto 0), (baseCounter ## concat32To64BitMemoryMux).asUInt),
                data    = io.wb.DAT_MOSI,
                write   = io.wb.CYC && io.wb.STB && io.wb.WE && writeToFrameBuffer && frameBufferSelected
            )
        
            dataFrameBuffer1 := frameBuffer1.readWriteSync(
                enable  = True,
                address = Mux(frameBufferSelected, (baseCounter ## concat32To64BitMemoryMux).asUInt, io.wb.ADR(11 downto 0)),
                data    = io.wb.DAT_MOSI,
                write   = io.wb.CYC && io.wb.STB && io.wb.WE && writeToFrameBuffer && !frameBufferSelected
            )
            
            when(concat32To64BitMemoryMux === 0) {
                dataMemConcatenated(31 downto 0) := Mux(frameBufferSelected, dataFrameBuffer1, dataFrameBuffer0)
            }
            
            .otherwise{
                dataMemConcatenated(63 downto 32) := Mux(frameBufferSelected, dataFrameBuffer1, dataFrameBuffer0)
            }

            data.rowData(getDataCounter-1) := dataMemConcatenated
            
            when(concat32To64BitMemoryMux === 0) {
                concat32To64BitMemoryMux := 1
            }
            .otherwise{
                getDataCounter := getDataCounter + 1
                concat32To64BitMemoryMux := 0;
            }
        }
    }
    .otherwise {
        getDataCounter := 0
        concat32To64BitMemoryMux := 0;
        io.dataResponse.valid := False
    }
}

//Hardware definition
class WishboneToRgbMatrix(config : WishboneConfig, baseBCMValue : Int, debug : Boolean) extends Component {
    val io = new Bundle {
        val RGB0 = out(RGBBundle())
        val RGB1 = out(RGBBundle())
        val Latch  = out Bool
        val Blank = out Bool
        val sclk = out Bool
        val Address = out UInt(5 bits)
        
        val wb = slave(Wishbone(config))
    }
    
    val Framebuffer = new WishboneFrameBuffer(config, debug)
    val dataCommand = Stream (rowNumberBundle())
    val dataResponse = Flow (rowBundle())
    Framebuffer.io.wb << io.wb
    dataCommand >-> Framebuffer.io.dataCommand
    Framebuffer.io.dataResponse >-> dataResponse
    
    dataCommand.valid := False
    dataCommand.payload.rowNumber := U"00000"
    dataCommand.payload.bcmNumber := U"000"
    
    io.RGB0.R := False
    io.RGB0.G := False
    io.RGB0.B := False
    io.RGB1.R := False
    io.RGB1.G := False
    io.RGB1.B := False
    
    
    val LedController = new StateMachine {
        val clockCounter = Reg(UInt(32 bits)) init (0)
        val dataOutputCounter = Reg(UInt(8 bits)) init (0)
        
        val rowCounter = Reg(UInt(5 bits)) init (0)
        val bcmCounter = Reg(UInt(3 bits)) init (2)
        
        val rowDataRed0 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        val rowDataGreen0 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        val rowDataBlue0 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        
        val rowDataRed1 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        val rowDataGreen1 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        val rowDataBlue1 = Reg(Bits(64 bits)) init (B"64'xFFFF00000000FFFF")
        
        val colCounter = Reg(UInt(5 bits)) init (0)
        val brightnessCounter = Reg(UInt(6 bits)) init (0)
        
        val sclkReg = Reg(Bool) init (False)
        
        io.Latch := False
        io.Blank := False
        
        io.Address := colCounter
        io.sclk := sclkReg
        
        val stateFetchData : State = new State with EntryPoint {
            onEntry{
                brightnessCounter := 0
                bcmCounter := bcmCounter + 1;
                
                when(bcmCounter === 7){
                    bcmCounter := 2;
                    rowCounter := rowCounter + 1
                    
                    when(rowCounter >= 31){
                        rowCounter := 0
                    }
                }
            }
            whenIsActive {
            
                dataCommand.valid := True 
                dataCommand.payload.rowNumber := rowCounter
                dataCommand.payload.bcmNumber := bcmCounter
                
                clockCounter := clockCounter + 1
                
                brightnessCounter := brightnessCounter + 1;
                when(brightnessCounter === 49){
                    brightnessCounter := 0
                }
                when(brightnessCounter <= Framebuffer.io.brightness){
                    io.Blank := True
                }
                .otherwise{
                    io.Blank := False
                }
                
                when(dataResponse.valid === True){
                   
                    rowDataRed0(63 downto 32) := dataResponse.payload.rowData(0)(63 downto 32)
                    rowDataRed1(63 downto 32) := dataResponse.payload.rowData(1)(63 downto 32)

                    rowDataRed0(31 downto 0) := dataResponse.payload.rowData(0)(31 downto 0)
                    rowDataRed1(31 downto 0) := dataResponse.payload.rowData(1)(31 downto 0)
  
                    rowDataGreen0(63 downto 32) := dataResponse.payload.rowData(2)(63 downto 32)
                    rowDataGreen1(63 downto 32) := dataResponse.payload.rowData(3)(63 downto 32)

                    rowDataGreen0(31 downto 0) := dataResponse.payload.rowData(2)(31 downto 0)
                    rowDataGreen1(31 downto 0) := dataResponse.payload.rowData(3)(31 downto 0)

                    rowDataBlue0(63 downto 32) := dataResponse.payload.rowData(4)(63 downto 32)
                    rowDataBlue1(63 downto 32) := dataResponse.payload.rowData(5)(63 downto 32)

                    rowDataBlue0(31 downto 0) := dataResponse.payload.rowData(4)(31 downto 0)
                    rowDataBlue1(31 downto 0) := dataResponse.payload.rowData(5)(31 downto 0)
                    
                }
                
                when(clockCounter === U(baseBCMValue, 32 bits)){
                    goto(stateTransmitData)
                }
                .elsewhen(clockCounter === U(baseBCMValue*3, 32 bits)){
                    goto(stateTransmitData)
                }
                .elsewhen(clockCounter === U(baseBCMValue*7, 32 bits)){
                    goto(stateTransmitData)
                }
                .elsewhen(clockCounter === U(baseBCMValue*15, 32 bits)){
                    goto(stateTransmitData)
                }
                .elsewhen(clockCounter === U(baseBCMValue*31, 32 bits)){
                    goto(stateTransmitData)
                }
                .elsewhen(clockCounter >= U(baseBCMValue*92, 32 bits)){
                    colCounter := colCounter + 1
                    clockCounter := 0
                    goto(stateTransmitData)
                }
            }
            onExit{
                
            }
        }
        
        val stateTransmitData : State = new State {
            onEntry{
                dataOutputCounter := 0
            }
            
            whenIsActive {
                io.Blank := True
                dataOutputCounter := dataOutputCounter + 1
                when((dataOutputCounter <= 127)){
                    sclkReg := !sclkReg
                    io.RGB0.R := rowDataRed0(dataOutputCounter(6 downto 1))
                    io.RGB0.G := rowDataGreen0(dataOutputCounter(6 downto 1))
                    io.RGB0.B := rowDataBlue0(dataOutputCounter(6 downto 1))
                    
                    io.RGB1.R := rowDataRed1(dataOutputCounter(6 downto 1))
                    io.RGB1.G := rowDataGreen1(dataOutputCounter(6 downto 1))
                    io.RGB1.B := rowDataBlue1(dataOutputCounter(6 downto 1))
                }
                .elsewhen(dataOutputCounter === 129){
                    io.Latch := True
                }
                .elsewhen(dataOutputCounter === 130){
                    io.Latch := False
                    goto(stateFetchData)
                }
            }
        }
    }
    
} 
