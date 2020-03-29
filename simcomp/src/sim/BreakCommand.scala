package sim

import sim.machine.AbstractMachine
import sim.unsigned.UInt

class BreakCommand extends Command {

  commandToken = "BREAK"
  commandDescription = "Set a breakpoint at address [ADDR]"
  commandHelpText = "BR<EAK> [ADDR]"

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length != 1) sb.append("SIM: Requires a breakpoint address.")
    else sim.Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        try {
          val addr = Integer.decode(tokenArray(0))
          m.addBreak(UInt(addr))
        }catch {
          case nfe:NumberFormatException =>
            sb.append("SIM: Illegal address.")
        }
    }
    false
  }
}

class UnBreakCommand extends Command {
  commandToken = "UNBREAK"
  commandDescription = "Remove any breakpoint at address [ADDR], or [ALL] for all breakpoints set"
  commandHelpText = "UNBR<EAK> [ADDR/ALL]"

  override def process(tokenArray: Array[String]): Boolean = {

    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length != 1) sb.append("SIM: Requires a breakpoint address, or ALL.")
    else sim.Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        if(tokenArray(0) == "ALL") m.clearBreaks()
        else
        try {
          val addr = Integer.decode(tokenArray(0))
          m.removeBreak(UInt(addr))
        }catch {
          case nfe:NumberFormatException =>
            sb.append("SIM: Illegal address.")
        }
    }
    false
  }
}