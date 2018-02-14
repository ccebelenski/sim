package com.sim.device

abstract class UnitOption(val optionName:String, val optionDescription:String) {

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
case class UnitOptionValue(var name:String, var value:Int)
class EnumValueUnitOption(optionName:String, optionDescription:String, val values:List[UnitOptionValue], var choice:String) extends  UnitOption(optionName:String,optionDescription:String) {

  override def copy: EnumValueUnitOption = {
    new EnumValueUnitOption(optionName,optionDescription,values,choice)
  }
}

class RangeValueUnitOption(optionName:String, optionDescription:String, val lowValue:Int, val highValue:Int, var currentValue:Int) extends UnitOption(optionName:String,optionDescription:String) {
  override def copy: RangeValueUnitOption = {
    new RangeValueUnitOption(optionName,optionDescription,lowValue,highValue,currentValue)
  }
}
