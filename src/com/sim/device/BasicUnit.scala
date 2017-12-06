package com.sim.device

import java.nio.file.Path

import com.sim.memory.AddressPointer
import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class BasicUnit(val device: BasicDevice) {

  var unitName:String
  var time: Long = 0L
  var usecs_remaining : Long = 0L



  device.addUnit(this)


  def init():Unit

  def action():Unit

}
