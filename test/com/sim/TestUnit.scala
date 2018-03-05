package com.sim

import com.sim.device.{BasicDevice, BasicUnit}
import com.sim.unsigned.{UByte, UInt}

class TestUnit(device:BasicDevice) extends BasicUnit(device: BasicDevice){

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = ???

  override def showCommand(sb:StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  override val waitTime: Long = 0
}
