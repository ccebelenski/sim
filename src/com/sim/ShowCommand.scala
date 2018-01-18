package com.sim

import com.sim.machine.AbstractMachine

class ShowCommand extends Command {

  commandToken = "SHOW"
  commandDescription = "Show available options for devices and other simulated resources"
  commandHelpText = "Shows more specific information about simulated resources."

  addSubCommand(new ShowMachineCommand)
  addSubCommand(new ShowDeviceCommand)

  override def process(tokenArray: Array[String]): Boolean = {
    if (tokenArray.length == 0) {
      Console.textTerminal.println(argsErrorMsg)
    } else {
      processSubCommand(tokenArray)
    }


    false
  }

}

class ShowMachineCommand extends Command {

  commandToken = "MACHINE"
  commandDescription = "Show details of the current simulated machine."
  commandHelpText = "Show machine details."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {

    Console.simEnvironment.simMachine match {
      case None => Utils.outln("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => m.showMachine
    }

    false
  }
}

class ShowDeviceCommand extends Command {
  commandToken = "DEVICE"
  commandDescription = "Show details of a device."
  commandHelpText = "Show device details."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {

    if (tokenArray.length == 0) {
      Utils.outln(s"SIM: Please specify a device.")
    } else Console.simEnvironment.simMachine match {
      case None => Utils.outln("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => {
        val devname = tokenArray(0)
        m.findDevice(devname) match {
          case None => Utils.outln(s"SIM: Device $devname not present.")
          case Some(v) => {

            v.showCommand()

          }
        }
      }
    }

    false
  }
}