package com.sim.s100

import com.sim.device.{BasicUnit, DiskUnit}
import com.sim.unsigned.{UByte, UInt}


class S100FD400Unit(device:S100FD400Device) extends BasicUnit(device) with  DiskUnit {

  override val DSK_SECTSIZE: Int = 137 // Size of sector
  override val DSK_SECT: Int = 32 // Sectors per track
  override val MAX_TRACKS: Int = 254 // Number of tracts, original Altair has 77 only

  var current_flag:Int = 0

  var sector_true :Int = 0


  override def writebuf(): Unit = {
    super.writebuf()
    current_flag &= 0xfe

  }

  override val waitTime: Long =0 // TODO

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {}

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def detach(): Unit = ???

}
