package com.sim.s100

import com.sim.cpu.{BasicCPU, Z80, Z80MMU}
import com.sim.machine.AbstractMachine
import com.sim.mux.MuxDevice
import com.sim.unsigned.UInt

class S100Machine extends AbstractMachine{


  override val description:String = "S100 machine."
  override val name = "S100"


  var cpu: Z80 = _


  override def init():Unit = {
    // Add the CPU first
    // NB the system timer will be added by the machine
    cpu = new Z80(false,this)
    addDevice(cpu)
    cpu.setMemorySize(UInt(0xFFFF))
    val mux = new MuxDevice(this)
    addDevice(mux)
    val sio = new S100SIODevice(this, cpu.MMU, List(UInt(0x00),UInt(0x01),UInt(0x02),UInt(0x03),UInt(0x10),UInt(0x11), UInt(0x12)))
    addDevice(sio)
    val fd = new S100FD400Device(this, cpu.MMU, List(UInt(0x08), UInt(0x09), UInt(0x0A)))
    addDevice(fd)

    val sim = new S100SIMDevice(this, cpu.MMU, List(UInt(0xfe)))
    addDevice(sim)
  }


  override def getCPU: Z80 = cpu
}
