package mylib 

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriStateArray
import spinal.lib.bus.wishbone._
import spinal.lib.com.uart._
import spinal.lib.com.spi._
import spinal.lib.fsm._

case class wbCmd(Addrwidth: Int, dataWidth: Int) extends Bundle {
  val address = Bits(Addrwidth bits)
  val data = Bits(dataWidth bits)
  val read = Bool
}

class SpiToFlow(conf : WishboneConfig) extends Component {
  val io = new Bundle{
    val spi = master(SpiSlave())
    val wbcommand = master(Flow(wbCmd(conf.addressWidth, conf.dataWidth)))
  }

  io.wbcommand.payload.address := (default -> false)
  io.wbcommand.payload.data := (default -> false)
  io.wbcommand.payload.read := True
  io.wbcommand.valid := False

  val spiCtrlConfig = SpiSlaveCtrlGenerics(
      dataWidth = 8)
      
  val spiCtrl = new SpiSlaveCtrl(spiCtrlConfig)
  spiCtrl.io.kind.cpol := False
  spiCtrl.io.kind.cpha := False
  spiCtrl.io.spi <> io.spi

  //default state for Uart, write not used atm
  spiCtrl.io.tx.valid := False 
  spiCtrl.io.tx.payload := B"8'x00"

  val fsm = new StateMachine{
    val dataVec = Reg(Vec(Bits(8 bits), 10))
    val counter = Reg(UInt(4 bits)) init 0
    val counterMax = UInt(4 bits)
    counterMax := (2 + (conf.addressWidth/8) + (conf.dataWidth / 8));

    val stateIdle : State = new State with EntryPoint {
      onEntry(counter := 0)
      whenIsActive {
        when(spiCtrl.io.rx.valid === True) {
          dataVec(counter) := spiCtrl.io.rx.payload 
          counter := counter + 1
        }

        when(counter === counterMax){  
          when(dataVec(0) === B"8'x01") {
            goto(stateWrite)
          }.elsewhen(dataVec(0) === B"8'x02") {
            //goto(stateRead)
          }
        }
      }
    }

    val stateWrite : State = new State {
      whenIsActive {
      
        //This is a scala for loop to parametrize the address and data payload.
        //This at least works with 16 and 32 bit for address and data but should for all widths
        val AddrWidthBytes = (conf.addressWidth/8)
        val DataWidthBytes = (conf.dataWidth/8)
        val StartOfDataOffset = 2
        for(x <- 0 until (AddrWidthBytes+DataWidthBytes)){
            if(x < AddrWidthBytes){
                val payloadOffset = (x*8)
                val dataVecOffset = (StartOfDataOffset+(AddrWidthBytes - 1 - x))
                io.wbcommand.payload.address((payloadOffset+7) downto (payloadOffset)) := dataVec(dataVecOffset)
            }
            else {
                val payloadOffset = (x - AddrWidthBytes)*8
                val dataVecOffset = ((StartOfDataOffset + AddrWidthBytes + DataWidthBytes) - ((x - AddrWidthBytes)+1))
                io.wbcommand.payload.data((payloadOffset+7) downto (payloadOffset)) := dataVec(dataVecOffset)
            }
        }
        
        io.wbcommand.payload.read := False
        io.wbcommand.valid := True
        goto(stateIdle)
      }
    }
  }
}

class UartToFlow(conf : WishboneConfig) extends Component {
  val io = new Bundle{
    val uart = master(Uart())
    val wbcommand = master(Flow(wbCmd(conf.addressWidth, conf.dataWidth)))
  }

  io.wbcommand.payload.address := (default -> false)
  io.wbcommand.payload.data := (default -> false)
  io.wbcommand.payload.read := True
  io.wbcommand.valid := False

  val uartCtrlConfig = UartCtrlGenerics(
      dataWidthMax = 8,
      clockDividerWidth = 16,
      preSamplingSize = 1,
      samplingSize = 5,
      postSamplingSize = 2)
      
  val uartCtrl = new UartCtrl(uartCtrlConfig)
  uartCtrl.io.config.clockDivider := 25  //100Mhz clock, 8 clocks per bit, 500KBaud, 39 for sim, 25 for rl
  uartCtrl.io.config.frame.dataLength := 7  //8 bits
  uartCtrl.io.config.frame.parity := UartParityType.NONE
  uartCtrl.io.config.frame.stop := UartStopType.ONE
  uartCtrl.io.uart <> io.uart

  //default state for Uart, write not used atm
  uartCtrl.io.write.valid := False 
  uartCtrl.io.writeBreak := False;
  uartCtrl.io.write.payload := B"8'x00"

  val fsm = new StateMachine{
    val dataVec = Reg(Vec(Bits(8 bits), 10))
    val counter = Reg(UInt(4 bits)) init 0
    val counterMax = UInt(4 bits)
    counterMax := (2 + (conf.addressWidth/8) + (conf.dataWidth / 8));

    val stateIdle : State = new State with EntryPoint {
      onEntry(counter := 0)
      whenIsActive {
        when(uartCtrl.io.read.valid === True) {
          dataVec(counter) := uartCtrl.io.read.payload 
          counter := counter + 1
        }

        when(counter === counterMax){  
          when(dataVec(0) === B"8'x01") {
            goto(stateWrite)
          }.elsewhen(dataVec(0) === B"8'x02") {
            //goto(stateRead)
          }
        }
      }
    }

    val stateWrite : State = new State {
      whenIsActive {
        //See comment above
        val AddrWidthBytes = (conf.addressWidth/8)
        val DataWidthBytes = (conf.dataWidth/8)
        val StartOfDataOffset = 2
        for(x <- 0 until (AddrWidthBytes+DataWidthBytes)){
            if(x < AddrWidthBytes){
                val payloadOffset = (x*8)
                val dataVecOffset = (StartOfDataOffset+(AddrWidthBytes - 1 - x))
                io.wbcommand.payload.address((payloadOffset+7) downto (payloadOffset)) := dataVec(dataVecOffset)
            }
            else {
                val payloadOffset = (x - AddrWidthBytes)*8
                val dataVecOffset = ((StartOfDataOffset + AddrWidthBytes + DataWidthBytes) - ((x - AddrWidthBytes)+1))
                io.wbcommand.payload.data((payloadOffset+7) downto (payloadOffset)) := dataVec(dataVecOffset)
            }
        }
        
        io.wbcommand.payload.read := False
        io.wbcommand.valid := True
        goto(stateIdle)
      }
    }
  }

}

class FlowToWishbone(conf : WishboneConfig) extends Component {
  val io = new Bundle{
    val wbcommand = slave(Flow(wbCmd(conf.addressWidth, conf.dataWidth)))
    val wbm = master(Wishbone(conf))
  }
  
  val wbInternal = Reg(Wishbone(conf))
  wbInternal.CYC init(False)
  wbInternal.STB init(False)
  wbInternal.WE init(False)
  wbInternal.ADR init(0)
  wbInternal.DAT_MOSI init(0)
  wbInternal.ACK init(False)
  wbInternal.DAT_MISO init(0)
  
  when(io.wbcommand.valid === True){
    wbInternal.ADR := io.wbcommand.payload.address.asUInt
    wbInternal.DAT_MOSI := io.wbcommand.payload.data
    wbInternal.WE := True
    wbInternal.CYC := True
    wbInternal.STB := True    
  }.elsewhen(io.wbm.ACK === True) {   //clear wishbone when Ack is received
    wbInternal.ADR := (default -> false)
    wbInternal.DAT_MOSI := (default -> false)
    wbInternal.WE := False
    wbInternal.CYC := False
    wbInternal.STB := False
  }

  io.wbm.CYC := wbInternal.CYC
  io.wbm.STB := wbInternal.STB
  wbInternal.ACK := io.wbm.ACK
  io.wbm.WE := wbInternal.WE
  io.wbm.ADR := wbInternal.ADR
  io.wbm.DAT_MOSI := wbInternal.DAT_MOSI
  wbInternal.DAT_MISO := io.wbm.DAT_MISO
  
}
