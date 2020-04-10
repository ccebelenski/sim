package sim.device

import sim.machine.AbstractMachine
import sim.unsigned.UInt

abstract class ConsoleDevice(machine:AbstractMachine) extends BasicDevice(machine) {
  override val description: String = "SIM virtual console device"
  override val name: String = "OP"


  override def init(): Unit = {
  }


}
