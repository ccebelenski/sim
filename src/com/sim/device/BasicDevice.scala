package com.sim.device

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicDevice {

  var name: String
  var description: String
  private val units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]

  def addUnit(unit:BasicUnit) : Unit = {
    units.append(unit)
  }

  def clearUnits(): Unit = {
    units.clear()
  }

  def getUnits(): Iterator[BasicUnit] = {
    units.toIterator
  }

}

object BasicDevice {

}