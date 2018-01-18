package com.sim

import java.util.ServiceLoader


import com.sim.machine.AbstractMachine

class ListCommand extends Command {

  commandToken = "LIST"
  commandDescription = "List available simulator features."
  commandHelpText = "Lists general features of the simulator, such as machine names."

  addSubCommand(new ListMachinesCommand)

  override def process(tokenArray: Array[String]): Boolean = {
    if(tokenArray.length == 0) {
      Console.textTerminal.println(argsErrorMsg)
    } else {
      processSubCommand(tokenArray)
    }


    false
  }
}

class ListMachinesCommand extends Command {
  commandToken = "MACHINES"
  commandDescription = "List available simulator machines."
  commandHelpText = "Lists the available machines the simulator can emulate."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {

    val sb:StringBuilder = new StringBuilder

    AbstractMachine.services.foreach(am => {
      sb.append(am.getName())
      sb.append("\t")
      sb.append(am.description)
      sb.append("\n")
    })

    Console.textTerminal.print(sb.toString())
    false
  }
}