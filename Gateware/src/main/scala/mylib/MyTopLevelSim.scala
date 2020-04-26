package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._

import scala.util.Random

//MyTopLevel's testbench
object MyTopLevelSimSpi {
def main(args: Array[String]) {
    val wbconf = new WishboneConfig(32,32);
    SimConfig.withWave.doSim(new RgbMatrixControllerSpi(wbconf, true)){dut =>
    //Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        SimTimeout(10*10000000)
        
        val baudPeriod = 50
        dut.io.spi.ss #= true
        dut.io.spi.sclk #= true
        sleep(baudPeriod*10)
        
        def tbToDut(buffer : Int) = {
            
            sleep(baudPeriod)

            (7 to 0 by -1).foreach{ bitId =>
            sleep(baudPeriod)
            dut.io.spi.sclk #= false
            sleep(baudPeriod)
            dut.io.spi.mosi #= ((buffer >> bitId) & 1) != 0
            sleep(baudPeriod)
            dut.io.spi.sclk #= true
            sleep(baudPeriod)
            }

            sleep(baudPeriod)
        }
        
        def testTbToDut(data : Int) ={
            val sendTxThread = fork(tbToDut(data))
            sendTxThread.join()
        }
        
        def WishboneWrite(address: Int, data: Int) = {
            
            testTbToDut(0x01)
            testTbToDut(0x01)
            
            for(x <- 0 to 3) {
            val dataPart: Int = address >> ((3-x) * 8)
            testTbToDut(dataPart)
            }
            
            for(x <- 0 to 3) {
            val dataPart: Int = data >> ((3-x) * 8)
            testTbToDut(dataPart)
            }
            
        }
        dut.io.spi.ss  #= false
        WishboneWrite(0x00000000, 0x0000000A)
        dut.clockDomain.waitSampling(1000)
        
        WishboneWrite(0x00004008, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)

        WishboneWrite(0x00004009, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)
        
        WishboneWrite(0x00004626, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)

        WishboneWrite(0x00004627, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)
        dut.io.spi.ss #= true
        dut.clockDomain.waitSampling(2500000)   //make waveform look a bit cleaner
    }
}
}

//MyTopLevel's testbench
/*
object MyTopLevelSim {
def main(args: Array[String]) {
    val wbconf = new WishboneConfig(32,32);
    SimConfig.withWave.doSim(new RgbMatrixController(wbconf)){dut =>
    //Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        
        SimTimeout(10*10000000)
        
        val baudPeriod = 2000
        
        dut.io.uart.rxd #= true
        
        def tbToDut(buffer : Int) = {
            dut.io.uart.rxd #= false
            sleep(baudPeriod)

            (0 to 7).foreach{ bitId =>
            dut.io.uart.rxd #= ((buffer >> bitId) & 1) != 0
            sleep(baudPeriod)
            }

            dut.io.uart.rxd #= true
            sleep(baudPeriod)
        }
        
        
        def testTbToDut(data : Int) ={
            val sendTxThread = fork(tbToDut(data))
            sendTxThread.join()
        }
        
        def WishboneWrite(address: Int, data: Int) = {
            testTbToDut(0x01)
            testTbToDut(0x01)
            
            for(x <- 0 to 3) {
            val dataPart: Int = address >> ((3-x) * 8)
            testTbToDut(dataPart)
            }
            
            for(x <- 0 to 3) {
            val dataPart: Int = data >> ((3-x) * 8)
            testTbToDut(dataPart)
            }
        }
        
        WishboneWrite(0x00000000, 0x0000000A)
        dut.clockDomain.waitSampling(1000)
        
        WishboneWrite(0x00004008, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)

        WishboneWrite(0x00004009, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)
        
        WishboneWrite(0x00004626, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)

        WishboneWrite(0x00004627, 0xFFFFFFFF)
        dut.clockDomain.waitSampling(1000)
        
        dut.clockDomain.waitSampling(2500000)   //make waveform look a bit cleaner
    }
}
}
*/
