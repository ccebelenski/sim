package com.sim.cpu

import com.sim.bus.BasicBus
import com.sim.memory.AddressPointer

/**
  * Created by christophercebelenski on 7/1/16.
  */
trait CPUState {

  var bus : BasicBus = _
  var addressPointer : AddressPointer = _

}
