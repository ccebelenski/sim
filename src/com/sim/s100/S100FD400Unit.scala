package com.sim.s100

import com.sim.device.{BasicUnit, DiskUnit}
import com.sim.unsigned.{UByte, UInt}


class S100FD400Unit(device:S100FD400Device) extends BasicUnit(device) with  DiskUnit {

  // Unit specific information
  var current_track:Int = 0
  var current_sector:Int = 0
  var current_byte:Int = 0
  var current_flag:Int = 0
  var sectors_per_track:Int  = device.DSK_SECT
  var tracks:Int = device.MAX_TRACKS


  var sector_true :Int = 0

  override val waitTime: Long =0 // TODO

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {}

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def detach(): Unit = ???

  override def readSector(): Unit = ???

  override def writeSector(): Unit = ???

  override def seek(pos: Long): Unit = ???
}
