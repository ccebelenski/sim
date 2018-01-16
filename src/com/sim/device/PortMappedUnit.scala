package com.sim.device

import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Ports are relative to a base port.  If the base port is 5, and the size = 2, then action port 5 is entry 0, and action port 6 is entry 1
  *
  * @param device
  * @param port
  * @param size
  */
abstract class PortMappedUnit(device: BasicDevice, val port: UInt, val size: UInt) extends BasicUnit(device) {

  override val isPortMapped: Boolean = true

  def handles(port: UInt): Boolean = {
    A.exists(a => a.port == port - this.port)
  }

  override def action(action: UInt, value: UInt): UInt = {
    A.find(a => a.port == action - this.port) match {
      case None => new UInt(0)
      case Some(a) => a.action(value)
    }
  }

  val A: ArrayBuffer[PortUnitAction] = new ArrayBuffer()

  case class PortUnitAction(port: UInt, action: (UInt) => UInt)

}
