package com.sim.s100

import com.sim.{Named, SimTimer}
import com.sim.cpu.Z80
import com.sim.machine.AbstractMachine

class S100Machine extends AbstractMachine{


  override val description:String = "S100 machine."
  override val name = "S100"




  override def init():Unit = {
    // Add the CPU - Initially the only device?
    // NB the system timer will be added by the machine
    devices.append(new Z80(true, this))
  }
}
