package com.sim.machine

import java.util.ServiceLoader

import com.sim.{Named, Utils}
import com.sim.device.BasicDevice

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

abstract class AbstractMachine extends Named{

  val description:String = "None"

  var devices: ListBuffer[BasicDevice] = new ListBuffer[BasicDevice]

  // device and machine names are always upper case
  override def getName(): String = super.getName().toUpperCase

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

}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = ServiceLoader.load(classOf[AbstractMachine]).asScala

}