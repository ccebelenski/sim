package com.sim.device

abstract class UnitOption(var optionName:String, var optionDescription:String) {

  def copy: UnitOption
}

class BinaryUnitOption(optionName:String, optionDescription:String, var value:Boolean) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Boolean = value

  override def copy: BinaryUnitOption = {
    new BinaryUnitOption(optionName, optionDescription, value)
  }
}

class ValueUnitOption(optionName:String, optionDescription:String, var value:Int) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Int = value

  override def copy: ValueUnitOption = {
    new ValueUnitOption(optionName,optionDescription,value)
  }
}
