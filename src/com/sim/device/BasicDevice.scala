package com.sim.device

import com.sim.machine.AbstractMachine
import com.sim.{Named, Utils}
import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicDevice(val machine:AbstractMachine) extends Named{

  val description: String
  private val units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]
  var awidth : UInt = UInt(0)
  // Device Identifier - first device is usually "A", second "B", etc.
  var deviceIdentifier:String = "A"
  private var enabled : Boolean = false

  // unit options - these are copied to each unit created.
  val unitOptions: ArrayBuffer[UnitOption] = new ArrayBuffer[UnitOption]
  //createUnitOptions



  // device and machine names are always upper case
  override def getName: String = super.getName.toUpperCase + deviceIdentifier

  def init() : Unit
  def createUnitOptions: Unit

  def addUnit(unit:BasicUnit) : Unit = synchronized {
    if(unit.device != this) throw new Exception("System check: Unit misconfiguration. Unit does not belong to device.")
    unit.unitNumber = findFirstFreeUnitNumber
    units.append(unit)
    unitOptions.foreach(duo => unit.unitOptions.append(duo.copy))
    unit.init()
  }

  def removeUnit(unit:BasicUnit) : Unit = synchronized {
    var unitsCopy = new ArrayBuffer[BasicUnit]
    unitsCopy.appendAll(units)
    units.clear()
    unitsCopy.foreach(u => {
      if(u != unit) units.append(u)
    })
    unitsCopy.clear() // prob not necessary
  }

  private def findFirstFreeUnitNumber: Int = {
    if(units.isEmpty) return 0
    val ulist = units.toList.sortBy(u => u.unitNumber)
    val length = ulist.length
    var left:Int = 0
    var right:Int = length -1
    while(left <= right) {
      val middle:Int = (right + left) >> 1
      if(ulist(middle).unitNumber != middle) {
        if(middle == 0 || ulist(middle -1).unitNumber == middle - 1) return middle
        right = middle -1
      } else left = middle + 1
    }
    length
  }

  def clearUnits(): Unit = synchronized {
    // TODO
    units.clear()
  }

  def getUnits: Iterator[BasicUnit] = synchronized {
    units.toIterator
  }

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"DEV: Device ${getName} Enabled: $state")
  }

  def isEnabled: Boolean = enabled


  def showCommand(sb:StringBuilder): Unit = {
    val dn = s"$getName: "
    sb.append(s"$dn$description\n")
    sb.append(s"${dn}Enabled: $isEnabled Units:${units.length}\n")
//    sb.append(s"${dn}aWidth: ${awidth.toHexString}\n")

    // Output default options
    sb.append(s"${dn}Default options:\n")
    unitOptions.foreach{uo => {
      uo.showOption(sb)
    }}

    sb.append(s"\n${dn}Units:\n")
    if(units.isEmpty) sb.append("\tNo Units.\n")
    units.foreach(u => {
      sb.append(s"  Unit: ${u.getName}\tenabled: ${u.isEnabled}\n")
    })

  }

  def getOption(optionName:String) : Option[UnitOption] = {
    unitOptions.find(p=> p.optionName == optionName)
  }

  // Called when the options have changed for a device.  This allows dynamic reloading when appropriate
  // Default is to do nothing.
  def optionsChanged(): Unit = {}

}

object BasicDevice {

}