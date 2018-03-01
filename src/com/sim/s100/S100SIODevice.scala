package com.sim.s100

import com.sim.device.{BasicDevice, BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine

class S100SIODevice(machine:AbstractMachine) extends BasicDevice(machine:AbstractMachine){

  override val description :String = "MITS 2SIO interface card"
  override val name = "SIO"

  override def init(): Unit = ???

  override def showCommand(sb: StringBuilder): Unit = ???

  override def createUnitOptions: Unit = {
    unitOptions.append(new BinaryUnitOption("TTY","Do not touch bit 8 of console output", value = false))
    unitOptions.append(new BinaryUnitOption("ANSI","Set bit 8 of console output to 0", value = false))
    unitOptions.append(new BinaryUnitOption("UPPER","Convert console input to upper case", value = false))
    unitOptions.append(new BinaryUnitOption("BS","Map delete to backspace", value = false))
    unitOptions.append(new BinaryUnitOption("QUIET","Display SIO error messages", value = false))
    unitOptions.append(new BinaryUnitOption("MAP","Enable mapping of characters", value = false))
    unitOptions.append(new BinaryUnitOption("BELL","Control-G sounds the bell", value = false))
    unitOptions.append(new BinaryUnitOption("SLEEP","Sleep after SIO status checks", value = false))
    unitOptions.append(new BinaryUnitOption("INTERRUPT","Status port 0 creates an interrupt when a character becomes available", value = false))
    unitOptions.append(new ValueUnitOption("IOPORT","Set I/O port to IOPORT", value = 0))

  }

  override def optionChanged(sb: StringBuilder): Unit = ???
}
