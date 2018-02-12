package com.sim

import com.sim.machine.AbstractMachine

class TestMachine extends AbstractMachine {
  override def init(): Unit = {
    devices.append(new TestDevice(this))
  }

}
