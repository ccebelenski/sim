package com.sim.device

import java.nio.file.Path

import com.sim.{Named, Utils}
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) extends Ordered[BasicUnit]{

  var time: Long = 0L
  var usecs_remaining : Long = 0L

  val isMemoryMapped = false
  val isPortMapped = false

  // Unit Number - First unit is typically 0, then 1, etc.
  var unitNumber:Int = 0

  // Is this unit a timer unit?
  var isTimerUnit:Boolean = false

  // Options for the unit, copied initially from the device defaults
  val unitOptions: ArrayBuffer[UnitOption] = new ArrayBuffer[UnitOption]

  private var enabled:Boolean = false
  var active: Boolean = false // Is the unit currently active? (processing event)

  // Attached path
  var attachedPath: Option[Path] = None
  // Does this unit support being attached?
  val supportsAttach: Boolean = false


  // device and machine names are always upper case
  def getName(): String = device.getName() + unitNumber

  // Does this unit handle requests for this port/memory address/etc?
  def handles(value: UInt) : Boolean
  // Perform a unit action
  def action(action: UInt, value: UByte, isWrite: Boolean) : UByte

  // Action is cancelled
  def cancel(): Unit

  // Complete an action (event queue)
  def completeAction(): Unit

  val dn = s"${getName()}:"

  device.addUnit(this)


  def init():Unit

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"UNIT: Unit ${getName()} Enabled: $state")
  }

  def isEnabled: Boolean = enabled

  override def compare(that: BasicUnit): Int = this.time.compareTo(that.time)

  override def compareTo(that: BasicUnit): Int = this.time.compareTo(that.time)

}
