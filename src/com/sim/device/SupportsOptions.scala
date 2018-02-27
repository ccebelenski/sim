package com.sim.device

import scala.collection.mutable.ArrayBuffer

trait SupportsOptions {

  // unit options - these are copied to each unit created.
  val unitOptions: ArrayBuffer[UnitOption] = new ArrayBuffer[UnitOption]

  def getOption(optionName:String) : Option[UnitOption] = {
    unitOptions.find(p=> p.optionName == optionName)
  }

  def setOption(optionName:String, optionValue: String, sb:StringBuffer): Unit = {
    // First find the option - if we don't find it, then we're done already
    val option = getOption(optionName)
    option match {
      case None => sb.append(s"Option $optionName is not valid and is ignored.")
      case Some(o:UnitOption) => {
        // Found a valid matching option - now try and set it
        o.setFromString(optionValue)

      }
    }
  }

}
