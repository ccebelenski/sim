package sim

import sim.cpu.BasicCPU
import sim.machine.AbstractMachine

class TestMachine extends AbstractMachine {
  override def init(): Unit = {
    devices.append(new TestDevice(this))
  }

  override def getCPU: BasicCPU = ???
}
