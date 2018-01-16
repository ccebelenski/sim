package com.sim.device

import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

abstract class MemoryMappedUnit(device: BasicDevice, val lowAddress: UInt, val highAddress: UInt) extends BasicUnit(device) {

  override val isMemoryMapped: Boolean = true
  def handles(address: UInt) : Boolean= {

    if(address <= highAddress && address >= lowAddress){
      A.exists(a => a.address == address)
    } else false
  }

  override def action(action: UInt, value: UInt): UInt = {
    A.find(a => a.address == action) match {
      case None => new UInt(0)
      case Some(a) => a.action(value)
    }
  }

  val A: ArrayBuffer[MappedUnitAction] = new ArrayBuffer()

  case class MappedUnitAction(address : UInt, action: (UInt) => UInt)

}

