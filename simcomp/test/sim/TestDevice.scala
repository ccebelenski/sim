package sim

import sim.device.BasicDevice
import sim.machine.AbstractMachine
import sim.unsigned.UInt

class TestDevice(machine:AbstractMachine) extends BasicDevice(machine:AbstractMachine) {

  override val description: String = "Test Device"

  override def handles(value: UInt): Boolean = ???

  override def init(): Unit = {}// would add devices here

  override def createUnitOptions: Unit = {} // nothing for now

  override def showCommand(sb: StringBuilder): Unit = ???

  override def optionChanged(sb: StringBuilder): Unit = ???
}
