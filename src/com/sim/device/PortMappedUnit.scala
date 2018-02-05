package com.sim.device

import com.sim.cpu.BasicMMU
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Ports are relative to a base port.  If the base port is 5, and the size = 2, then action port 5 is entry 0, and action port 6 is entry 1
  *
  * @param device
  * @param ports List of ports this unit responds to
  * @param size
  */
abstract class PortMappedUnit(device: BasicDevice, mmu: BasicMMU, val ports: List[UInt], val size: UInt) extends BasicUnit(device) {

  override val isPortMapped: Boolean = true

  def handles(port: UInt): Boolean = {
    ports.contains(port)
  }

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte =
  {
    actions.get(action) match {
      case None => UByte(0)
      case Some(a) => a.action(value, isWrite)
    }
  }
  val actions = new mutable.HashMap[UInt, PortUnitAction]()

  case class PortUnitAction(port: UInt, action: (UInt, Boolean) => UByte)

  override def setEnable(state: Boolean): Unit = {
    super.setEnable(state)
    if(state) {
      // Enabling
     mmu.mapPortMappedUnit(this)
    } else {
      // Disabling
      mmu.unMapIOPort(this)
    }
  }

}
