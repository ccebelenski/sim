package com.sim

import com.sim.machine.AbstractMachine

class SetCommand  extends Command {

  commandToken = "SET"
  commandDescription = "Set simulator features."
  commandHelpText = "Set features of the simulator, such as machines, devices, etc."

  addSubCommand(new SetMachineCommand)

  override def process(tokenArray: Array[String]): Boolean = {
    if(tokenArray.length == 0) {
      Console.textTerminal.println(argsErrorMsg)
    } else {
      processSubCommand(tokenArray)
    }


    false
  }

}


class SetMachineCommand extends Command {
  commandToken = "MACHINE"
  commandDescription = "Set (create) the machine to be simulated."
  commandHelpText = "Defines the machine that will simulated."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {

    if(tokenArray.size != 1) Utils.outln(s"SIM: Please specify a machine.")
    else {
      val mn = tokenArray(0)
      AbstractMachine.services.find(am => am.getName == mn) match {
        case None => Utils.outln(s"SIM: Machine $mn not valid.")
        case Some(x) =>
          Console.simEnvironment.simMachine = Some(x)
          x.init()
          Utils.outln(s"SIM: Machine $mn defined.")
      }
    }

    false
  }

}