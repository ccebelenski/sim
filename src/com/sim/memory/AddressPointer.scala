package com.sim.memory

import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/1/16.
  */
class AddressPointer(var addressSpace: AddressSpace) {
  private var address: UInt = UInt(0)

  def get() : UInt = address

  def set(uInt: UInt) : Unit = {
    address = uInt
    AddressPointer.checkRange(addressSpace, this)
  }

}

object AddressPointer {

  def checkRange(addressSapce: AddressSpace, address : AddressPointer) : Unit = {
    AddressSpace.checkRange(address,addressSapce)
  }
}
