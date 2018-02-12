package com.sim.machine

import java.util.ServiceLoader

import com.sim._
import com.sim.device.BasicDevice

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

abstract class AbstractMachine extends Named{

  val description:String = "None"

  var devices: ListBuffer[BasicDevice] = new ListBuffer[BasicDevice]

  // Create a new system event queue
  val eventQueue : EventQueue = new EventQueue


  // device and machine names are always upper case
  override def getName(): String = super.getName().toUpperCase


  // Set up the master timer device - always present
  SimTimer.sim_timer_init() // set up some universal stuff.
  val simTimerDevice = new SimTimer(this)
  val masterTimer = new SimTimerUnit(simTimerDevice, true)
  masterTimer.init() // Init will define this as the master timer. (calibrated)
  this.devices.append(simTimerDevice)

  Utils.outln(s"SIM: OS Tick:${SimTimer.sim_os_tick_hz}Hz\tIdle Rate:${SimTimer.sim_idle_rate_ms}ms\tClock Res:${SimTimer.sim_os_clock_resolution_ms}ms")

  /**
    * Show command for SHOW MACHINE
    */
  def showMachine : Unit = {

    val sb = new StringBuilder
    sb.append(s"SIM: Simulated Machine: ${getName()} : $description\n")
    sb.append(s"SIM: Available devices:\n")
    if(devices.isEmpty) sb.append(s"SIM: \t No devices.\n")
    devices.foreach(d => sb.append(s"SIM: \t ${d.getName()}\tEna: ${d.isEnabled}\n"))

    Utils.outln(sb.toString())
  }


  def findDevice(deviceName:String) : Option[BasicDevice] = {
    devices.find(d => d.getName().equalsIgnoreCase(deviceName))
  }

  def init() : Unit
}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = ServiceLoader.load(classOf[AbstractMachine]).asScala

}