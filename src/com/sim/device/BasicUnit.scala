package com.sim.device

import java.nio.file.Path

import com.sim.unsigned.{UByte, UInt}

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) {

  var unitName:String
  var time: Long = 0L
  var usecs_remaining : Long = 0L

  val isMemoryMapped = false
  val isPortMapped = false

  // Does this unit handle requests for this port/memory address/etc?
  def handles(value: UInt) : Boolean
  // Perform a unit action
  def action(action: UInt, value: UByte, isWrite: Boolean) : UByte


  device.addUnit(this)


  def init():Unit

}
