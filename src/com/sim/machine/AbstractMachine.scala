package com.sim.machine

import java.util.ServiceLoader

import com.sim._
import com.sim.cpu.BasicCPU
import com.sim.device.{BasicDevice, BasicUnit, PortMappedDevice}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

abstract class AbstractMachine extends Named{

  val description:String = "None"

  var devices: ListBuffer[BasicDevice] = new ListBuffer[BasicDevice]

  // Create a new system event queue
  val eventQueue : EventQueue = new EventQueue


  // device and machine names are always upper case
  override def getName: String = super.getName.toUpperCase


  // Set up the master timer device - always present
  SimTimer.sim_timer_init() // set up some universal stuff.
  val simTimerDevice = new SimTimer(this)
  addDevice(simTimerDevice)
  val masterTimer = new SimTimerUnit(simTimerDevice, true)
  simTimerDevice.addUnit(masterTimer)
  // If there was no master timer already (true in this case) the timer will cause itself to do that. This
  // guarantees there's only one master timer, created here.


  Utils.outln(s"SIM: OS Tick:${SimTimer.sim_os_tick_hz}Hz\tIdle Rate:${SimTimer.sim_idle_rate_ms}ms\tClock Res:${SimTimer.sim_os_clock_resolution_ms}ms")

  /**
    * Show command for SHOW MACHINE
    */
  def showMachine : Unit = {

    val sb = new StringBuilder
    sb.append(s"SIM: Simulated Machine: ${getName} : $description\n")
    sb.append(s"SIM: Available devices:\n")
    if(devices.isEmpty) sb.append(s"SIM: \t No devices.\n")
    devices.foreach(d => sb.append(s"SIM: \t ${d.getName}\tEna: ${d.isEnabled}\n"))

    Utils.outln(sb.toString())
  }


  def findDevice(deviceName:String) : Option[BasicDevice] = {
    devices.find(d => d.getName.equalsIgnoreCase(deviceName))
  }

  def findUnitDevice(deviceName:String) : Option[BasicUnit] = {
    var result: Option[BasicUnit] = None
    devices.foreach( d=> {
      d.getUnits.find( u => u.getName.equalsIgnoreCase(deviceName)) match {
        case None => {}
        case Some(x:BasicUnit) => result = Some(x)
        case _ => throw new Exception("System check: Unknown findUnitDevice")
      }
    })

    result
  }

  def addDevice(device:BasicDevice) : Unit = {
    // TODO Name management
    device.createUnitOptions
    device.init()
    device match {
      case device1: PortMappedDevice => getCPU.MMU.mapPortMappedDevice(device1)
      case _ =>
    }

    devices.append(device)
  }

  def removeDevice(deviceName:String) : Unit = {
    // TODO
  }

  def init() : Unit

  def getCPU: BasicCPU

}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = ServiceLoader.load(classOf[AbstractMachine]).asScala

}