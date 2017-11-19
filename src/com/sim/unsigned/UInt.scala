package com.sim.unsigned
import scala.math.{ScalaNumber, ScalaNumericConversions}
/**
  * Created by christophercebelenski on 7/18/16.
  */
class UInt(val intValue: Int) extends AnyVal with SmallUInt[UInt] {
  override def toUInt: UInt = this
  override def intRep: Int = intValue
}

object UInt {
  def MinValue = UInt(0)
  def MaxValue = UInt(~0)

  def apply(x: Int) = new UInt(x)
  def unapply(x: UInt) = Some(x.intValue)
}
