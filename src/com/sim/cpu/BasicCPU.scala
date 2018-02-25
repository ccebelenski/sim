package com.sim.cpu

import java.util.function.Consumer

import com.sim.{SimTimer, SimTimerUnit, Utils}
import com.sim.device.{BasicDevice, BinaryUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicCPU(val isBanked: Boolean, override val machine: AbstractMachine) extends BasicDevice(machine) {

  val KBLOG2 = UInt(10)
  val KB = UInt(1024)

  // Clock frequency, in Khz.  0 = as fast as possible
  protected var clockFrequency: UInt = UInt(0)
  protected var clockHasChanged = true

  def setClockFrquency(freq: UInt): Unit = {
    clockFrequency = freq
    clockHasChanged = true
    Utils.outln(s"CPU: Clock frequency changed to: ${clockFrequency}Khz")
  }

  def runcpu(): Unit // Main CPU execution loop

  def onHalt(): Unit // called when CPU is about to be halted and returning to cmd line

  // Unit options common to all CPU's.
  override def createUnitOptions: Unit = {
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Stop on halt instruction.", value = false))

  }

  @inline
  final def setFlag(reg:Register8, flag:Int, clear:Boolean) : Unit = {
    if(clear) reg(reg & ~flag) else reg(reg | flag)
  }
  @inline
  final def setFlag(reg:Register16, flag:Int, clear:Boolean) : Unit = {
    if(clear) reg(reg & ~flag) else reg(reg | flag)
  }
  @inline
  final def testFlag(reg:Register8, flag:Int) : Boolean = {
    if((reg & flag) != 0) true else false
  }
  @inline
  final def testFlag(reg:Register16, flag:Int): Boolean = {
    if((reg & flag) != 0) true else false
  }

  val registers: Map[String, Register]


  class X extends Consumer[String] {
    override def accept(t: String): Unit = ???
  }

  val MMU: BasicMMU

  private var MEMORYSIZE: UInt = UInt(0)

  def setMemorySize(size: UInt): Unit = {
    val maxsize = if (isBanked) MMU.MAXMEMORY else MMU.MAXBANKSIZE
    var newsize = size << KBLOG2
    if (isBanked) newsize = newsize & ~MMU.ADDRMASK
    if (newsize < KB) newsize = KB
    if (newsize > maxsize) newsize = maxsize
    MEMORYSIZE = newsize
    awidth = MMU.MAXBANKSIZELOG2
    if (newsize > MMU.MAXBANKSIZE) awidth = awidth + MMU.MAXBANKSLOG2

    Utils.outln(s"SIM: Memory size = ${Utils.formatBytes(MEMORYSIZE.toLong,true)} Banked: $isBanked")

  }

  def getMemorySize: UInt = MEMORYSIZE

  def resetCPU(): Unit

  /* UI Routines */
  def examine(address: Int): UByte = {
    val byte = MMU.get8(address)
    Utils.outln(f"SIM: 0x$address%04X:0x${byte.byteValue}%02X")
    byte
  }

  def examineWord(address: Int): UShort = {
    val word = MMU.get16(address)
    Utils.outln(f"SIM: 0x$address%04X:0x${word.shortValue}%04X")
    word
  }

  def examineRegister(nmemonic: String): Int = {
    registers.get(nmemonic) match {
      case Some(r: Register8) =>
        Utils.outln(f"SIM: $r")
        r.get8.intValue
      case Some(r: CompositeRegister16) =>
        Utils.outln(f"SIM: $r")
        r.get16.intValue
      case Some(r: Register16) =>
        Utils.outln(f"SIM: $r")
        r.get16.intValue
      case _ =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
        0
    }
  }

  def deposit(address: Int, byte: UByte): Unit = {
    MMU.put8(address, byte)

  }

  def deposit(address:Int, byte: Int) : Unit = {
    MMU.put8(address, UByte((byte & 0xff).byteValue()))
  }


  def depositWord(address: Int, word: UShort): Unit = {
    MMU.put16(address, word)
  }

  def depositWord(address:Int, word:Int): Unit = {
    MMU.put16(address, UShort((word & 0xffff).shortValue))
  }

  def showRegisters(): String
  def showFlags():String

  def setRegister8(nmemonic: String, value: UByte): Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
      case Some(r: Register8) =>
        r.set8(value)
      case _ =>
        Utils.outln(s"SIM: Register is not an 8 bit register.")
    }
  }

  def setRegister16(nmemonic: String, value: UShort): Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
      case Some(r: Register16) =>
        r.set16(value)
      case _ =>
        Utils.outln(s"SIM: Register is not a 16 bit register.")
    }
  }


  override def showCommand(stringBuilder: StringBuilder) : Unit = {
    super.showCommand(stringBuilder)
    stringBuilder.append(s"$getName> Registers:\n" + showRegisters() + "\n")
    stringBuilder.append(s"$getName> Flags:\n" + showFlags() + "\n")
  }
}

abstract class Register(val nmenomic: String) {
  val aWidth: Int
}

class Register8(override val nmenomic: String) extends Register(nmenomic) {
  private var value: UByte = UByte(0)

  @inline
  def get8 = value

  @inline
  def set8(value: UByte): Unit = this.value = value

  @inline
  def set8(value: Register8): Unit = set8(value.get8)

  //@inline
  // def set8(value: Byte): Unit = set8(UByte(value))

  @inline
  def increment(): Unit = value = new UByte((value.byteValue + 1).toByte)

  @inline
  def decrement(): Unit = value = new UByte((value.byteValue - 1).toByte)

  override val aWidth = 8

  override def toString: String = f"$nmenomic:0x${value.intValue}%02X"

  def apply(value: UByte): Unit = set8(value)

  def apply(value: Register8): Unit = set8(value.get8)

  def apply(value: Int): Unit = set8(UByte((value & 0xff).byteValue()))

  def +(value: Int): Byte = {
    ((this.value + value) & 0xff).byteValue()
  }

  def -(value: Int): Byte = {
    ((this.value - value) & 0xff).byteValue()
  }

  def &(value: Int): Int = {
    this.value & value
  }

  def |(value: Int): Int = {
    this.value | value
  }
  def ^(value: Int): Int = {
    this.value ^ value
  }

  def >>(value: Int): Int = {
    (this.get8 >> value) & 0xff
  }

  def <<(value: Int): Int = {
    (this.get8 << value) & 0xff
  }
}

object Register8 {
  implicit def reg82UByte(reg8: Register8): UByte = reg8.get8

  implicit def reg82Byte(reg8: Register8): Byte = reg8.get8.byteValue

  implicit def reg82Int(reg8: Register8): Int = reg8.get8.intValue
}

// In the case of HL, H is most significant, L is least.  L would be written to memory first because Z80 is little endian
class CompositeRegister16(override val nmenomic: String, val high: Register8, val low: Register8) extends Register16(nmenomic) {
  @inline
  def get8high: UByte = high.get8

  @inline
  def get8low: UByte = low.get8

  @inline
  override def get16: UShort = (low.get8  | (high.get8 << 8)).toUShort

  @inline
  override def set16(value: UShort): Unit = {
    low.set8(UByte((value & 0xff).toByte))
    high.set8(UByte(((value >> 8) & 0xFF).toByte))
  }

  @inline
  override def set16(value: Register16): Unit = {
    set16(value.get16)
  }

  @inline
  def set8high(value: UByte): Unit = {
    high.set8(value)
  }

  @inline
  def set8low(value: UByte): Unit = {
    low.set8(value)
  }


  override val aWidth = 16

  override def toString: String = f"$nmenomic:0x${get16.intValue}%04X"
}

class Register16(override val nmenomic: String) extends Register(nmenomic) {
  private var value: UShort = UShort(0)

  @inline
  def get16: UShort = value

  @inline
  def set16(value: UShort): Unit = this.value = value

  @inline
  def set16(value: Register16): Unit = this.value = value.get16

  @inline
  def set16(value:Int): Unit = this.value = UShort(value.shortValue())

  @inline
  def increment(): Unit = set16(UShort((get16 + 1).shortValue()))

  @inline
  def decrement(): Unit = set16(UShort((get16 - 1).shortValue()))

  def swap(register16: Register16): Unit = {
    val temp = register16.get16
    register16.set16(this.get16)
    this.set16(temp)
  }

  def +(value: Int): Int = {
    (this.get16 + value) & 0xffff
  }

  def -(value: Int): Int = {
    (this.get16 - value) & 0xffff
  }

  def &(value: Int): Int = {
    this.get16 & value
  }

  def |(value: Int): Int = {
    this.get16 | value
  }

  def >>(value: Int): Int = {
    (this.get16 >> value) & 0xffff
  }

  def <<(value: Int): Int = {
    (this.get16 << value) & 0xffff
  }

  def ^(value: Int): Int = {
    this.get16 ^ value
  }

  override val aWidth = 16

  override def toString: String = f"$nmenomic:0x${value.intValue}%04X"

  def apply(value: UShort): Unit = set16(value)

  def apply(value: Register16): Unit = set16(value.get16)

  def apply(value: Int): Unit = set16(UShort((value & 0xffff).shortValue()))

  //def apply(value: UInt): Unit = set16(value.shortValue)
}

object Register16 {
  implicit def reg162UShort(value: Register16): UShort = value.get16

  implicit def reg162Short(value: Register16): Short = value.get16.shortValue

  implicit def reg162Int(value: Register16): Int = value.get16.toInt

  //implicit def int2reg16(value: Int): Register16 = new Register16(value)
}