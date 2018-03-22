package com.sim

import com.sim.machine.AbstractMachine
import com.sim.unsigned.UInt

class GoCommand extends Command {
  commandToken = "GO"
  commandDescription = "Execute starting at address <start>, or last position."
  commandHelpText = "GO <START>"
  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder

    Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        if (tokenArray.length >= 1)
          try {
            val addr = Integer.decode(tokenArray(0))
            m.getCPU.runcpu(false,UInt(addr))
          } catch {
            case nfe: NumberFormatException =>
              sb.append("SIM: Illegal start address.")
          }
        else {
          // No start address, just restart the CPU
          m.getCPU.runcpu()
        }
    }
    Utils.outln(sb.toString())
    false
  }

}

class StepCommand extends Command {
  commandToken = "STEP"
  commandDescription = "Single-step the CPU"
  commandHelpText = "STEP - execute one instruction on the CPU"
  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        m.getCPU.runcpu(singleStep = true)

    }
    Utils.outln(sb.toString())
    false
  }
}
