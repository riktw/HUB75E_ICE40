/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package mylib

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.wishbone._
import spinal.lib.bus.misc._
import spinal.lib.com.uart._
import spinal.lib.com.spi._
import scala.util.Random


class RgbMatrixController(config : WishboneConfig) extends Component {
    val io = new Bundle {
        val RGB0 = out(RGBBundle())
        val RGB1 = out(RGBBundle())
        val Latch  = out Bool
        val Blank = out Bool
        val sclk = out Bool
        val Address = out UInt(5 bits)
        
        val uart = master(Uart())
    }
    
    val wbRgbMatrix = new WishboneToRgbMatrix(config, 50, false)
    val wbuart =  new UartToFlow(config)
    val wbControl = new FlowToWishbone(config)
    
    io.RGB0 := wbRgbMatrix.io.RGB0
    io.RGB1 := wbRgbMatrix.io.RGB1
    io.Latch := wbRgbMatrix.io.Latch
    io.Blank := wbRgbMatrix.io.Blank
    io.sclk := wbRgbMatrix.io.sclk
    io.Address := wbRgbMatrix.io.Address
    
    io.uart <> wbuart.io.uart
    wbControl.io.wbcommand << wbuart.io.wbcommand
    wbControl.io.wbm >> wbRgbMatrix.io.wb
}

class internalClock() extends BlackBox{
    val io = new Bundle {
        val intclk = out Bool
    }
}

class RgbMatrixControllerSpi(config : WishboneConfig, debug : Boolean) extends Component {
    val io = new Bundle {
        val RGB0 = out(RGBBundle())
        val RGB1 = out(RGBBundle())
        val Latch  = out Bool
        val Blank = out Bool
        val sclk = out Bool
        val Address = out UInt(5 bits)
        val clk = in Bool
        
        val spi = master(SpiSlave())
    }
    
    val iClk = Bool 
    val intClk = new internalClock()
    iClk := intClk.io.intclk
    
    val myClockDomain = ClockDomain(
    clock  = iClk,
    config = ClockDomainConfig(
      clockEdge        = RISING,
      resetKind        = BOOT
    )
  )
    val myArea = new ClockingArea(myClockDomain){
    val wbRgbMatrix = new WishboneToRgbMatrix(config, 50, debug)
    val wbSpi =  new SpiToFlow(config)
    val wbControl = new FlowToWishbone(config)
    
    io.RGB0 := wbRgbMatrix.io.RGB0
    io.RGB1 := wbRgbMatrix.io.RGB1
    io.Latch := wbRgbMatrix.io.Latch
    io.Blank := wbRgbMatrix.io.Blank
    io.sclk := wbRgbMatrix.io.sclk
    io.Address := wbRgbMatrix.io.Address
    
    io.spi <> wbSpi.io.spi
    wbControl.io.wbcommand << wbSpi.io.wbcommand
    wbControl.io.wbm >> wbRgbMatrix.io.wb
    }
}


//Generate the MyTopLevel's Verilog
object MyTopLevelVerilogSpi {
  def main(args: Array[String]) {
    val wbconf = new WishboneConfig(16,32); 
    SpinalVerilog(new RgbMatrixControllerSpi(wbconf, false))
  }
}
