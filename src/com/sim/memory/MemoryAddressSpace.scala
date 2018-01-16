package com.sim.memory

import com.sim.Console
import com.sim.unsigned.{UByte, UInt}

class MemoryAddressSpace(lowAddress: UInt, highAddress: UInt) extends AddressSpace(lowAddress, highAddress) {

  override val isReadOnly: Boolean = false

  private val M: Array[UInt] = new Array[UInt]((highAddress - lowAddress).toInt)
  private val tt = Console.textTerminal


  override def put8(address: UInt, value: UInt): Unit = {
    if (address <= lowAddress || address >= highAddress) {
      tt.println(s"Memory: Illegal memory write access. Addr: ${address.toHexString}")
    } else {


    }
  }

  override def get8(address: UInt): UByte = ???

  override def get16(address: UInt): UInt = ???
}
