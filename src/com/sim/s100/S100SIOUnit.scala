package com.sim.s100

import com.sim.device.{BasicDevice, BasicUnit, MuxUnitAware}
import com.sim.unsigned.{UByte, UInt}


class S100SIOUnit(device: S100SIODevice) extends BasicUnit(device: BasicDevice)
  with MuxUnitAware {
  override def init(): Unit = {


  }


  override val waitTime:Long = 100000L

  override def cancel(): Unit = ???

  override def completeAction(): Unit = {
    if (inputCharacterWaiting) {
      device.machine.getCPU.keyboardInterrupt = true
    }

  }

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  var inputCharacter:Int = -1
  var inputCharacterWaiting:Boolean = false


}