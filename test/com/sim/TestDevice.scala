package com.sim

import com.sim.device.BasicDevice
import com.sim.machine.AbstractMachine

class TestDevice(machine:AbstractMachine) extends BasicDevice(machine:AbstractMachine) {

  override val description: String = "Test Device"

  override def init(): Unit = {}// would add devices here

  override def createUnitOptions: Unit = {} // nothing for now

  override def showCommand(sb: StringBuilder): Unit = ???

  override def optionChanged(sb: StringBuilder): Unit = ???
}
