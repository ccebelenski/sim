package sim

import sim.device.Bootable
import sim.machine.AbstractMachine

class BootCommand extends Command {
  commandToken = "BOOT"
  commandDescription = "Attempts to boot a specific unit."
  commandHelpText = "Boot a unit.  The unit must be bootable and be attached to bootable media."

  override def process(tokenArray: Array[String]): Boolean = {
    val sb:StringBuilder = new StringBuilder
    var willBoot = false
    if (tokenArray.length == 0) {
      sb.append(s"SIM: Please specify a unit.")
    } else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => {
        val devname = tokenArray(0)
        m.findUnitDevice(devname) match {
          case None =>
            // Device unit not found...
            sb.append(s"SIM: Device unit $devname not found.")
          case Some(v) =>
            // Get the Device for this unit, see if it's bootable
            val dev = v.device
            if(!dev.isInstanceOf[Bootable]) {
              sb.append(s"SIM: Device $devname is not a boot device.")

            } else {
              dev.asInstanceOf[Bootable].boot(v.unitNumber,sb)
              willBoot = true
            }

        }
      }
    }


    Utils.outln(sb.toString)
    if(willBoot) {} // TODO start the CPU

    false
  }


}
