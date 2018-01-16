package com.sim.altair

import com.sim.Named
import com.sim.cpu.Z80
import com.sim.machine.AbstractMachine

class AltairMachine extends AbstractMachine{


  override val description:String = "Altair Z80."
  var name = "AltairZ80"

  // Add the CPU - Initially the only device?
  devices.append(new Z80(true))




}
