package com.sim.device

import java.nio.file.Path

import com.sim.{Named, Utils}
import com.sim.unsigned.{UByte, UInt}

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) extends Named{

  var time: Long = 0L
  var usecs_remaining : Long = 0L

  val isMemoryMapped = false
  val isPortMapped = false

  private var enabled:Boolean = false

  // device and machine names are always upper case
  override def getName(): String = super.getName().toUpperCase

  // Does this unit handle requests for this port/memory address/etc?
  def handles(value: UInt) : Boolean
  // Perform a unit action
  def action(action: UInt, value: UByte, isWrite: Boolean) : UByte

  val dn = s"${getName()}:"

  device.addUnit(this)


  def init():Unit

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"UNIT: Unit ${getName()} Enabled: $state")
  }

  def isEnabled: Boolean = enabled
}
