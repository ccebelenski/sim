package com.sim.device

abstract class UnitOption(val optionName: String, val optionDescription: String) {

  def copy: UnitOption

  def formatValue: String

  def showOption(sb: StringBuilder): Unit = {
    sb.append(s"  Option: ${optionName.toUpperCase} \t\t$optionDescription\t$formatValue\n")
  }

  def setFromString(s: String, sb: StringBuilder): Boolean

  def optionHelp: String
}

case class BinaryUnitOption(override val optionName: String, override val optionDescription: String, var value: Boolean) extends UnitOption(optionName: String, optionDescription: String) {
  def getValue: Boolean = value

  override def formatValue: String = s"${if (!value) "NO" else ""}${optionName.toUpperCase}"

  override def copy: BinaryUnitOption = {
    new BinaryUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: StringBuilder): Boolean = {
    try {
      value = s.toBoolean
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is binary.  Valid values are TRUE and FALSE."
}

case class ValueUnitOption(override val optionName: String, override val optionDescription: String, var value: Int) extends UnitOption(optionName: String, optionDescription: String) {
  def getValue: Int = value

  override def copy: ValueUnitOption = {
    new ValueUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: StringBuilder): Boolean = {
    try {
      value = s.toInt
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are integer numbers."

  override def formatValue: String = s"$value"
}

class UnitOptionValue(var name: String, var value: Int)

case class EnumValueUnitOption(override val optionName: String, override val optionDescription: String, values: List[UnitOptionValue], var choice: String) extends UnitOption(optionName: String, optionDescription: String) {

  override def copy: EnumValueUnitOption = {
    new EnumValueUnitOption(optionName, optionDescription, values, choice)
  }

  override def optionHelp: String = {
    val sb = new StringBuilder
    sb.append(s"${optionName.toUpperCase} is value.  Valid values are:\n")
    values.foreach(x => sb.append(s"${x.name}\n"))

    sb.toString()
  }

  override def setFromString(s: String, sb: StringBuilder): Boolean = ???

  override def formatValue: String = s"[${values.toString}]"
}

case class RangeValueUnitOption(override val optionName: String, override val optionDescription: String, lowValue: Int, highValue: Int, var currentValue: Int) extends UnitOption(optionName: String, optionDescription: String) {
  override def copy: RangeValueUnitOption = {
    new RangeValueUnitOption(optionName, optionDescription, lowValue, highValue, currentValue)
  }

  override def setFromString(s: String, sb: StringBuilder): Boolean = ???

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are integer numbers in range $lowValue to $highValue"

  override def formatValue: String = s"$lowValue to $highValue"
}

case class StringValueUnitOption(override val optionName: String, override val optionDescription: String, var value: String) extends UnitOption(optionName, optionDescription) {
  override def copy: StringValueUnitOption = {
    new StringValueUnitOption(optionName, optionDescription, value)
  }

  override def setFromString(s: String, sb: StringBuilder): Boolean = {
    try {
      value = s
      true
    } catch {
      case t: Throwable =>
        sb.append(optionHelp)
        false
    }
  }

  override def optionHelp: String = s"${optionName.toUpperCase} is value.  Valid values are a string."

  override def formatValue: String = s"$value"

}
