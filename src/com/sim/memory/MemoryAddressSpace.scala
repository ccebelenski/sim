package com.sim.memory

import com.sim.{Console, Utils}
import com.sim.unsigned.{UByte, UInt}

class MemoryAddressSpace(lowAddress: UInt, highAddress: UInt) extends AddressSpace(lowAddress, highAddress) {

  override val isReadOnly: Boolean = false

  private val M: Array[UByte] = new Array[UByte]((highAddress - lowAddress).toInt)
  private val tt = Console.textTerminal


  override def put8(address: UInt, value: UByte): Unit = {
    if (address <= lowAddress || address >= highAddress) {
      Utils.outln(s"Memory: Illegal memory write access. Addr: ${address.toHexString}")
    } else {
      M(address.toInt) = value

    }
  }

  override def get8(address: UInt): UByte = {
    if (address <= lowAddress || address >= highAddress) {
      Utils.outln(s"Memory: Illegal memory read access. Addr: ${address.toHexString}")
      UByte(0)
    } else {
      M(address.toInt)
    }
  }


}
