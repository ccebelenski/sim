package com.sim.s100

import com.sim.cpu.Z80MMU
import com.sim.device.{Bootable, PortMappedDiskDevice, SupportsOptions}
import com.sim.unsigned.{UByte, UInt}

class S100HDSKDevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDiskDevice(machine, mmu, ports)
  with SupportsOptions with Bootable  {

  override val description: String = "Hard Disk"
  override val name = "HD"
  override val supportsBoot: Boolean = true

  override def init(): Unit = {
    // Create 16 units
    for (i <- 0 until S100HDSKDevice.HDSK_NUMBER) {
      val du = new S100HDSKUnit(this)
      addUnit(du)
    }

  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???
}

object S100HDSKDevice {
  val HDSK_CAPACITY: Long = 2048 * 32 * 128 //Default Altair HDSK Capacity
  val HDSK_NUMBER: Int = 16 //number of hard disks
  val SPT16: Int = 16
  val SPT32: Int = 32
  val SPT26: Int = 26
  val SPT52: Int = 52
}