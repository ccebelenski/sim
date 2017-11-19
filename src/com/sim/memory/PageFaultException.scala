package com.sim.memory

class PageFaultException(addressSpace: AddressSpace, address: AddressPointer) extends AddressSpaceException(addressSpace = addressSpace) {

}
