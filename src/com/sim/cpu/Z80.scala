package com.sim.cpu
import com.sim.unsigned.UInt

class Z80(isBanked:Boolean) extends BasicCPU(isBanked) {
  override var deviceName: String = "Z80 CPU"
  override val MMU: BasicMMU = new Z80MMU(this)

  override def init(): Unit = ???
}
