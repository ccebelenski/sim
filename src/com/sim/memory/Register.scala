package com.sim.memory

import com.sim.Named
import com.sim.unsigned.UInt

abstract class Register() extends Named {

  def get(): UInt

  def set(): UInt

}
