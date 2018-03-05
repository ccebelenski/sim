package com.sim.device

import com.sim.cpu.BasicMMU
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Ports are relative to a base port.  If the base port is 5, and the size = 2, then action port 5 is entry 0, and action port 6 is entry 1
  *
  * @param machine
  * @param ports List of ports this unit responds to
  */
abstract class PortMappedDevice(machine: AbstractMachine, mmu: BasicMMU, val ports: List[UInt]) extends BasicDevice(machine) {

  override val isPortMapped: Boolean = true
  val A: ArrayBuffer[MappedDeviceAction] = new ArrayBuffer()

  def handles(port: UInt): Boolean = {
    ports.contains(port)
  }

  override def setEnable(state: Boolean): Unit = {
    super.setEnable(state)
    if(state) {
      // Enabling
     mmu.mapPortMappedDevice(this)
    } else {
      // Disabling
      mmu.unMapIOPort(this)
    }
  }

  def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    A.find(a => a.address == action) match {
      case None => new UByte(0)
      case Some(a) => a.action(value, isWrite)
    }
  }

}
