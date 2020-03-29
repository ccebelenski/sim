package sim.memory

import sim.unsigned.UInt

class PageFaultException(addressSpace: AddressSpace, address: UInt) extends AddressSpaceException(addressSpace = addressSpace) {

}
