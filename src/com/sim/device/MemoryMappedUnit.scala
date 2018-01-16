package com.sim.device

import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable.ArrayBuffer

abstract class MemoryMappedUnit(device: BasicDevice, val lowAddress: UInt, val highAddress: UInt) extends BasicUnit(device) {

  override val isMemoryMapped: Boolean = true
  def handles(address: UInt) : Boolean= {

    if(address <= highAddress && address >= lowAddress){
      A.exists(a => a.address == address)
    } else false
  }

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    A.find(a => a.address == action) match {
      case None => new UByte(0)
      case Some(a) => a.action(value, isWrite)
    }
  }

  val A: ArrayBuffer[MappedUnitAction] = new ArrayBuffer()

  case class MappedUnitAction(address : UInt, action: (UInt, Boolean) => UByte)

}

