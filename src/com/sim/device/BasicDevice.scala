package com.sim.device

import com.sim.machine.AbstractMachine
import com.sim.{Named, Utils}
import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicDevice(val machine:AbstractMachine) extends Named{

  val description: String = "No Description Available"
  private val units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]
  var awidth : UInt = UInt(0)
  // Device Identifier - first device is usually "A", second "B", etc.
  var deviceIdentifier:String = "A"
  private var enabled : Boolean = false

  // unit options - these are copied to each unit created.
  val unitOptions: ArrayBuffer[UnitOption] = new ArrayBuffer[UnitOption]
  //createUnitOptions



  // device and machine names are always upper case
  override def getName(): String = super.getName().toUpperCase + deviceIdentifier

  def init() : Unit
  def createUnitOptions: Unit

  def addUnit(unit:BasicUnit) : Unit = {
    if(unit.device != this) throw new Exception("System check: Unit misconfiguration. Unit does not belong to device.")
    units.append(unit)
    unitOptions.foreach(duo => unit.unitOptions.append(duo.copy))
    unit.init()
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
    Utils.outln(s"DEV: Device ${getName} Enabled: $state")
  }

  def isEnabled: Boolean = enabled


  def showCommand(sb:StringBuilder): Unit = {
    val dn = s"${getName()}: "
    sb.append(s"$dn$description\n")
    sb.append(s"${dn}Enabled: $isEnabled Units:${units.length}\n")
//    sb.append(s"${dn}aWidth: ${awidth.toHexString}\n")

    // Output default options
    sb.append(s"${dn}Default options:\n")
    unitOptions.foreach{uo => {
      uo.showOption(sb)
    }}

    sb.append(s"\n${dn}\tUnits:\n\n")
    if(units.isEmpty) sb.append(s"$dn\tNo Units.\n")
    units.foreach(u => {
      sb.append(s"$dn\tUnit: ${u.getName}\tenabled: ${u.isEnabled}\n")
    })

  }

}

object BasicDevice {

}