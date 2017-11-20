package com.sim.bus

import com.sim.Named
import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicBus(val busSize:UInt) extends Named{


  def addRegion(region:BusRegion)
  def removeRegion(region:BusRegion)

  def toString():String



}
