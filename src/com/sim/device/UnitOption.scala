package com.sim.device

abstract class UnitOption(val optionName:String, val optionDescription:String) {

  def copy: UnitOption
  def formatValue:String
  def showOption(sb:StringBuilder): Unit = {
    sb.append(s"  Option: ${optionName.toUpperCase} \t\t$optionDescription\t$formatValue\n")
  }
}

class BinaryUnitOption(optionName:String, optionDescription:String, var value:Boolean) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Boolean = value

  override def formatValue: String = s"${if(!value)"NO" else ""}${optionName.toUpperCase}"
  override def copy: BinaryUnitOption = {
    new BinaryUnitOption(optionName, optionDescription, value)
  }

}

class ValueUnitOption(optionName:String, optionDescription:String, var value:Int) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Int = value

  override def copy: ValueUnitOption = {
    new ValueUnitOption(optionName,optionDescription,value)
  }

  override def formatValue: String = s"$value"
}
case class UnitOptionValue(var name:String, var value:Int)
class EnumValueUnitOption(optionName:String, optionDescription:String, val values:List[UnitOptionValue], var choice:String) extends  UnitOption(optionName:String,optionDescription:String) {

  override def copy: EnumValueUnitOption = {
    new EnumValueUnitOption(optionName,optionDescription,values,choice)
  }

  override def formatValue: String = s"[${values.toString}]"
}

class RangeValueUnitOption(optionName:String, optionDescription:String, val lowValue:Int, val highValue:Int, var currentValue:Int) extends UnitOption(optionName:String,optionDescription:String) {
  override def copy: RangeValueUnitOption = {
    new RangeValueUnitOption(optionName,optionDescription,lowValue,highValue,currentValue)
  }

  override def formatValue: String = s"$lowValue to $highValue"
}
