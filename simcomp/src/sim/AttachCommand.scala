package sim

import sim.device.UnitAttachable
import sim.machine.AbstractMachine

class AttachCommand extends Command {
  commandToken = "ATTACH"
  commandDescription = "Attaches an image to a unit."
  commandHelpText = "Attach an image to a unit.  The unit must be attachable (eg, disk, tape, etc)."

  override def process(tokenArray: Array[String]): Boolean = {

    if (tokenArray.length == 0)
      Utils.outln(argsErrorMsg)
    else if (tokenArray.length != 2)
      Utils.outln("SIM: Invalid args: ATTACH <UNIT> <FILE>")
    else {
      // Correct arg count, lets get the specs
      val unitName = tokenArray(0)
      val fileName = tokenArray(1)
      Console.simEnvironment.simMachine match {
        case None => Utils.outln("SIM: No machine.  SET a MACHINE.")
        case Some(m: AbstractMachine) =>

          m.findUnitDevice(unitName) match {
            case None => Utils.outln(s"${m.getName}: Unit $unitName not found.")
            case Some(unit) =>
              if (!unit.isInstanceOf[UnitAttachable]) {
                Utils.outln(s"${m.getName} Unit is not attachable.")
                return true
              }
              val sb = new StringBuilder
              unit.asInstanceOf[UnitAttachable].attach(fileName, sb)
              Utils.outln(sb.toString())
          }

      }
    }


    false
  }
}
