package com.sim

import com.sim.device.BasicDevice
import com.sim.machine.AbstractMachine

class TestDevice(machine:AbstractMachine) extends BasicDevice(machine:AbstractMachine) {
  override def init(): Unit = {}// would add devices here

  override def createDefaultUnitOptions: Unit = {} // nothing for now

  override def showCommand(sb: StringBuilder): Unit = ???
}
