package com.sim.s100

import com.sim.device.{BasicUnit, DiskUnit}

class S100HDSKUnit(device:S100HDSKDevice) extends BasicUnit(device) with  DiskUnit {

  // Format type - actually the parameters for the type
  var HDSK_FORMAT_TYPE : Option[S100HDiskParamsBase] = None




  override val waitTime: Long = 0L

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {
    // TODO
  }

  override def MAX_TRACKS: Int = ???

  override def DSK_SECT: Int = ???

  override def DSK_SECTSIZE: Int = ???

  override def optionChanged(sb: StringBuilder): Unit = ???
}
