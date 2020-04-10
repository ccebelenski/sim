package sim.s100

import sim.device.{BasicDevice, BasicUnit, ConsoleUnit, MuxUnitAware}


class S100SIOUnit(device: S100SIODevice) extends ConsoleUnit(device: BasicDevice) {

  override val waitTime:Long = 100000L

  override def cancel(): Unit = ???

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: StringBuilder): Unit = ???

}