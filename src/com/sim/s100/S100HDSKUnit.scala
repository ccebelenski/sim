package com.sim.s100

import com.sim.device.{BasicUnit, DiskUnit}

class S100HDSKUnit(device:S100HDSKDevice) extends BasicUnit(device) with  DiskUnit {
  override val waitTime: Long = 0L

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = ???

  override def MAX_TRACKS: Int = ???

  override def DSK_SECT: Int = ???

  override def DSK_SECTSIZE: Int = ???

  override def optionChanged(sb: StringBuilder): Unit = ???
}
