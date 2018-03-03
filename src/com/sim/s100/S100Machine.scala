package com.sim.s100

import com.sim.cpu.{BasicCPU, Z80, Z80MMU}
import com.sim.machine.AbstractMachine
import com.sim.mux.MuxDevice

class S100Machine extends AbstractMachine{


  override val description:String = "S100 machine."
  override val name = "S100"


  var cpu: Z80 = _


  override def init():Unit = {
    // Add the CPU - Initially the only device?
    // NB the system timer will be added by the machine
    val cpu = new Z80(true,this)
    addDevice(cpu)
    val mux = new MuxDevice(this)
    addDevice(mux)
    val sio = new S100SIODevice(this)
    addDevice(sio)

  }


  override def getCPU: Z80 = cpu
}
