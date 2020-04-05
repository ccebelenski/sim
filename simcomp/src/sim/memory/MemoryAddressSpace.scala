package sim.memory

import sim.unsigned.{UByte, UInt}
import sim.{Console, Utils}

import scala.language.existentials

class MemoryAddressSpace(lowAddress: UInt, highAddress: UInt) extends AddressSpace(lowAddress, highAddress) {

  override val isReadOnly: Boolean = false

  private val M: Array[UByte] = new Array[UByte]((highAddress - lowAddress + 1))

  for(x <- 0 to (highAddress - lowAddress).toInt) M(x) = UByte(0)

  override def put8(address: UInt, value: UByte): Unit = {
    if (address < lowAddress || address > highAddress) {
      Utils.outln(s"Memory: Illegal memory write access. Addr: ${address.toHexString}")
    } else {
      M(scaleAddress(address)) = value

    }
  }

  override def get8(address: UInt): UByte = {
    if (address < lowAddress || address > highAddress) {
      Utils.outln(s"Memory: Illegal memory read access. Addr: ${address.toHexString}")
      UByte(0)
    } else {
      M(scaleAddress(address))
    }
  }

  @inline
  private def scaleAddress(address:UInt) : Int = {
    M.length - (highAddress - address) - 1
  }

}

class ROMAddressSpace(lowAddress: UInt, highAddress: UInt) extends MemoryAddressSpace(lowAddress, highAddress) {
  override val isReadOnly: Boolean = true

  override def put8(address:UInt, value:UByte): Unit = {
      Utils.outln(s"Memory: Illegal ROM write access. Addr: ${address.toHexString}")
  }

  // One-time load for ROM's
  def load8(address:UInt, value:UByte) : Unit = {
    super.put8(address,value)
  }

}