package com.sim.device

import com.sim.{Named, Utils}
import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicDevice extends Named{

  val description: String = "No Description Available"
  val units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]
  var awidth : UInt = UInt(0)
  private var enabled : Boolean = false

  // device and machine names are always upper case
  override def getName(): String = super.getName().toUpperCase

  def init() : Unit

  def addUnit(unit:BasicUnit) : Unit = {
    units.append(unit)
  }

  def clearUnits(): Unit = {
    units.clear()
  }

  def getUnits(): Iterator[BasicUnit] = {
    units.toIterator
  }

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"DEV: Device ${getName()} Enabled: $state")
  }

  def isEnabled: Boolean = enabled


  def showCommand(): Unit = {
    val dn = s"${getName()}: "
    val sb = new StringBuilder
    sb.append(s"$dn$description\n")
    sb.append(s"${dn}Enabled: $isEnabled Units:${units.length}\n")
    sb.append(s"${dn}aWidth: ${awidth.toHexString}")
    units.foreach(u => {
      sb.append(s"$dn\tUnit: ${u.getName()}\t${u.isEnabled}")
    })

  }
}

object BasicDevice {

}