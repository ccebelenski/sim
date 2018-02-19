package com.sim.s100

import com.sim.{Named, SimTimer}
import com.sim.cpu.Z80
import com.sim.machine.AbstractMachine
import com.sim.mux.MuxDevice

class S100Machine extends AbstractMachine{


  override val description:String = "S100 machine."
  override val name = "S100"




  override def init():Unit = {
    // Add the CPU - Initially the only device?
    // NB the system timer will be added by the machine
    val cpu = new Z80(true,this)
    addDevice(cpu)
    val mux = new MuxDevice(this)
    addDevice(mux)
  }
}
