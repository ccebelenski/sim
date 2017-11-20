package com.sim.cpu

import com.sim.memory.AddressPointer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicCPU {

  def step()

  def setAddress(addressPointer: AddressPointer)

  def init(cPUState: CPUState, addressPointer: AddressPointer)

  def interrupt(interrupt: Interrupt)

  def run()


}
