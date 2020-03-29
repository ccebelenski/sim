package sim

import java.lang.management.{ManagementFactory, MemoryType}
import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Date

import sim.machine.AbstractMachine

//import scala.collection.JavaConverters._
import scala.jdk.CollectionConverters._

class ShowCommand extends Command {

  commandToken = "SHOW"
  commandDescription = "Show available options for devices and other simulated resources"
  commandHelpText = "Shows more specific information about simulated resources."

  addSubCommand(new ShowMachineCommand)
  addSubCommand(new ShowDeviceCommand)
  addSubCommand(new ShowBreakpointsCommand)
  addSubCommand(new ShowMLogsCommand)
  addSubCommand(new ShowJVMCommand)

  override def process(tokenArray: Array[String]): Boolean = {
    if (tokenArray.length == 0) {
      sim.Console.textTerminal.println(argsErrorMsg)
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

    sim.Console.simEnvironment.simMachine match {
      case None => Utils.outln("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>

        m.showMachine
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

    val sb: StringBuilder = new StringBuilder
    if (tokenArray.length == 0) {
      sb.append(s"SIM: Please specify a device.")
    } else sim.Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) => {
        val devname = tokenArray(0)
        m.findDevice(devname) match {
          case None =>
            // Device not found, look for a unit with that name.
            m.findUnitDevice(devname) match {
              case None => sb.append(s"SIM: Device $devname not present.")
              case Some(u) => u.showCommand(sb)
            }


          case Some(v) => {

            v.showCommand(sb)

          }
        }
      }
    }


    Utils.outln(sb.toString)
    false
  }
}

class ShowBreakpointsCommand extends Command {
  commandToken = "BREAKPOINTS"
  commandDescription = "Show set breakpoints."
  commandHelpText = "Show breakpoints that have been set."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder

    sim.Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        m.showBreaks(sb)
    }
    Utils.outln(sb.toString())
    false
  }
}

class ShowMLogsCommand extends Command {
  commandToken = "MLOGS"
  commandDescription = "Show set memory logpoints."
  commandHelpText = "Show memory logpoints that have been set."
  level = 1

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder

    sim.Console.simEnvironment.simMachine match {
      case None => sb.append("SIM: No machine.  SET a MACHINE.")
      case Some(m: AbstractMachine) =>
        m.showMemLogs(sb)
    }
    Utils.outln(sb.toString())
    false
  }
}

class ShowJVMCommand extends Command {
  commandToken = "JVM"
  commandDescription = "Show JVM stats."
  commandHelpText = "Show some JVM related information for debugging and monitoring purposes."

  override def process(tokenArray: Array[String]): Boolean = {
    val sb: StringBuilder = new StringBuilder
    val runtime = Runtime.getRuntime
    level = 1

    // Clean up stuff first - this should be the only place we do this.
    //runtime.gc()

    val processorCount = runtime.availableProcessors()
    val freeMemory = runtime.freeMemory()
    val totalMemory = runtime.totalMemory()
    val maxMemory = runtime.maxMemory()

    var totalGarbageCollections = 0L
    var garbageCollectionTime = 0L

    ManagementFactory.getGarbageCollectorMXBeans.asScala.foreach(x => {
      totalGarbageCollections += x.getCollectionCount
      garbageCollectionTime += x.getCollectionTime
    })

    val jvmUpTime = ManagementFactory.getRuntimeMXBean.getUptime
    val jvmStartTime = ManagementFactory.getRuntimeMXBean.getStartTime
    val jvmVersion = ManagementFactory.getRuntimeMXBean.getVmVersion
    val jvmVendor = ManagementFactory.getRuntimeMXBean.getVmVendor
    val jvmName = ManagementFactory.getRuntimeMXBean.getVmName

    sb.append(s"$jvmName $jvmVersion ($jvmVendor)\n")
    sb.append(s"Up: ${Instant.ofEpochMilli(jvmStartTime).toString} - running for ${new SimpleDateFormat("mm:ss:SSS").format(new Date(jvmUpTime))}\n")
    sb.append(s"\nMemory stats:\n\tTotal:\t${Utils.formatBytes(totalMemory,false)}\n\tFree:\t${Utils.formatBytes(freeMemory,false)}\n\tMax:\t${Utils.formatBytes(maxMemory,false)}\n\n")
    sb.append(s"CPU and garbage collection stats:\n\tCPU:\t\t$processorCount\n\tGCs:\t\t$totalGarbageCollections\n\tGC Time:\t$garbageCollectionTime\n\n")

    sb.append(s"Memory pool stats:\n")
    ManagementFactory.getMemoryPoolMXBeans().asScala.filter(_.getType == MemoryType.HEAP).foreach(x => {
      sb.append(s"\t${x.getName}:\n\t\t${x.getUsage}\n")
    })

    Utils.out(sb.toString())
    false
  }
}