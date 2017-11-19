package com.sim.memory

import com.sim.Named
import com.sim.memory.AddressSpaceType.AddressSpaceType
import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/18/16.
  */
class AddressSpace(val lowAddress: UInt, val highAddress: UInt) extends Named{

  // Default to undefined address space type
  var addressSpaceType : AddressSpaceType = AddressSpaceType.INIT
  // Is the address space mapped currently?
  var isMapped: Boolean = false

  /**
    * Returns true if address space contains the address.
    * @param address
    * @return
    */
  def containsAddress(address:AddressPointer) : Boolean = {

    val x: UInt = address.get()
    if( (x >= lowAddress) && (x <= highAddress)) true else false
  }

}

object AddressSpace {

  def checkRange(value: AddressPointer, checkSpace : AddressSpace) : Unit = {

    if(!checkSpace.containsAddress(value) || (!checkSpace.isMapped)) throw new PageFaultException(checkSpace, value)

  }

}

object AddressSpaceType extends Enumeration {
  type AddressSpaceType = Value
  val RAM, ROM, Bus, INIT = Value
}
