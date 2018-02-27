package com.sim.device

import java.nio.file.Path

import com.sim.{Named, Utils}
import com.sim.unsigned.{UByte, UInt}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) extends Ordered[BasicUnit] with SupportsOptions {

  var time: Long = 0L
  var usecs_remaining : Long = 0L

  val isMemoryMapped = false
  val isPortMapped = false

  // Unit Number - First unit is typically 0, then 1, etc.
  var unitNumber:Int = 0

  // Is this unit a timer unit?
  var isTimerUnit:Boolean = false

  private var enabled:Boolean = false
  var active: Boolean = false // Is the unit currently active? (processing event)

  // Attached path
  var attachedPath: Option[Path] = None
  // Does this unit support being attached?
  val supportsAttach: Boolean = false

  val dn = s"$getName:"

  def showCommand(sb:StringBuilder): Unit = {

    sb.append(s"${dn}  Unit ${getName} Active: ${active}\n")

    if(supportsAttach && attachedPath.isDefined) sb.append(s"$dn Attached: ${attachedPath.get.getFileName.toString}\n")

    sb.append(s"${dn}Unit options:\n")
    unitOptions.foreach{uo => {
      uo.showOption(sb)
    }}
  }

  // device and machine names are always upper case
  def getName: String = device.getName + unitNumber

  // Does this unit handle requests for this port/memory address/etc?
  def handles(value: UInt) : Boolean
  // Perform a unit action
  def action(action: UInt, value: UByte, isWrite: Boolean) : UByte

  // Action is cancelled
  def cancel(): Unit

  // Complete an action (event queue)
  def completeAction(): Unit


  // Called when the options have changed for a unit.  This allows dynamic reloading when appropriate
  // Default is to do nothing.
  def optionsChanged(): Unit = {}

  def init():Unit

  // General enable function - devices can implement their own for device specific things
  def setEnable(state: Boolean) : Unit = {
    enabled = state
    Utils.outln(s"UNIT: Unit ${getName} Enabled: $state")
  }

  def isEnabled: Boolean = enabled

  override def compare(that: BasicUnit): Int = this.time.compareTo(that.time)

  override def compareTo(that: BasicUnit): Int = this.time.compareTo(that.time)

}
