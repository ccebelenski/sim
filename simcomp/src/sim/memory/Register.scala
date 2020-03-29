package sim.memory

import sim.Named
import sim.unsigned.UInt

abstract class Register() extends Named {

  def get(): UInt

  def set(): UInt

}
