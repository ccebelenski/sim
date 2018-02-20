package com.sim.device

abstract class UnitOption(val optionName:String, val optionDescription:String) {

  def copy: UnitOption
  def formatValue:String
  def showOption(sb:StringBuilder): Unit = {
    sb.append(s"  Option: ${optionName.toUpperCase} \t\t$optionDescription\t$formatValue\n")
  }
}

case class BinaryUnitOption(override val optionName:String, override val optionDescription:String, var value:Boolean) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Boolean = value

  override def formatValue: String = s"${if(!value)"NO" else ""}${optionName.toUpperCase}"
  override def copy: BinaryUnitOption = {
    new BinaryUnitOption(optionName, optionDescription, value)
  }

}

case class ValueUnitOption(override val optionName:String, override val optionDescription:String, var value:Int) extends UnitOption(optionName:String,optionDescription:String) {
  def getValue: Int = value

  override def copy: ValueUnitOption = {
    new ValueUnitOption(optionName,optionDescription,value)
  }

  override def formatValue: String = s"$value"
}
class UnitOptionValue(var name:String, var value:Int)
case class EnumValueUnitOption(override val optionName:String,override val  optionDescription:String, values:List[UnitOptionValue], var choice:String) extends  UnitOption(optionName:String,optionDescription:String) {

  override def copy: EnumValueUnitOption = {
    new EnumValueUnitOption(optionName,optionDescription,values,choice)
  }

  override def formatValue: String = s"[${values.toString}]"
}

case class RangeValueUnitOption(override val optionName:String, override val optionDescription:String, lowValue:Int, highValue:Int, var currentValue:Int) extends UnitOption(optionName:String,optionDescription:String) {
  override def copy: RangeValueUnitOption = {
    new RangeValueUnitOption(optionName,optionDescription,lowValue,highValue,currentValue)
  }

  override def formatValue: String = s"$lowValue to $highValue"
}

case class StringValueUnitOption(override val optionName:String, override val optionDescription:String, var value:String) extends UnitOption(optionName,optionDescription) {
  override def copy: StringValueUnitOption = {
    new StringValueUnitOption(optionName,optionDescription,value)
  }

  override def formatValue: String = s"$value"

}
