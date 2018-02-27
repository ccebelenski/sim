package com.sim

import com.sim.device.{SupportsOptions, UnitOption}
import com.sim.machine.AbstractMachine

import scala.collection.mutable.ArrayBuffer

class SetCommand  extends Command {

  commandToken = "SET"
  commandDescription = "Set simulator features."
  commandHelpText = "Set features of the simulator, such as machines, devices, etc."

  addSubCommand(new SetMachineCommand)
  addSubCommand(new SetDeviceCommand)

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

    if (tokenArray.size != 1) Utils.outln(s"SIM: Please specify a machine.")
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
  class SetDeviceCommand extends Command {
    commandToken = "DEVICE"
    commandDescription = "Set device specific attributes."
    commandHelpText = "Set attributes of devices that affect their operations."
    level = 1

    override def process(tokenArray: Array[String]): Boolean = {
    val sb:StringBuilder = new StringBuilder
    if (tokenArray.length == 0) {
      sb.append(s"SIM: Please specify a device.")
    } else Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => {
        val devname = tokenArray(0)


        m.findDevice(devname) match {
          case None =>
            // Device not found, look for a unit with that name.
            m.findUnitDevice(devname) match {
              case None =>  sb.append(s"SIM: Device $devname not present.")
              case Some(u) =>
                if(tokenArray.size == 1) return false // No options - nothing to do
              val options = tokenArray.slice(1,tokenArray.size)
              val opts = parseOpts(options)
            }


          case Some(v) => {
            if(tokenArray.size == 1) return false // No options - nothing to do
            val options = tokenArray.slice(1,tokenArray.size)
            val opts = parseOpts(options)

          }
        }
      }
    }


    Utils.outln(sb.toString)
    false
  }

    private def setOptions(x:SupportsOptions, o: List[(String, String)], sb:StringBuffer) : Unit = {
      o.foreach(z => {
        x.setOption(z._1, z._2, sb)
      })
    }

    private def parseOpts(tokenArray: Array[String]) : List[(String,String)] = {
      val ab = new ArrayBuffer[(String,String)]
      // TODO

      ab.toList
    }
}