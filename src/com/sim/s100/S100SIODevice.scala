package com.sim.s100

import com.sim.device._
import com.sim.machine.AbstractMachine

class S100SIODevice(machine:S100Machine) extends BasicDevice(machine:AbstractMachine) with MuxAware with SerialDevice {

  override val description :String = "MITS 2SIO interface card"
  override val name = "SIO"

  override def init(): Unit = {
    //super.init()

    // Create a default serial console unit
    val unit = new S100SIOUnit(this, machine.getCPU.MMU, List())
    this.addUnit(unit)

  }

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)

  }

  override def createUnitOptions: Unit = {
    createSerialUnitOptions

    unitOptions.append(BinaryUnitOption("INTERRUPT","Status port 0 creates an interrupt when a character becomes available", value = false))
    unitOptions.append(ValueUnitOption("IOPORT","Set I/O port to IOPORT", value = 0))

  }

  override def optionChanged(sb: StringBuilder): Unit = ???
}
