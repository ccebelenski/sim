package com.sim.device

import com.sim.Named

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicDevice extends Named{

  val description: String = "No Description Available"
  private val units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]
  var deviceName:String

  def init() : Unit

  def addUnit(unit:BasicUnit) : Unit = {
    units.append(unit)
  }

  def clearUnits(): Unit = {
    units.clear()
  }

  def getUnits: Iterator[BasicUnit] = {
    units.toIterator
  }

}

object BasicDevice {

}