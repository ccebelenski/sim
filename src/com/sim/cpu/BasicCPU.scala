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
  protected var clockFrequency: Int = 0
  protected var clockHasChanged: Boolean = true

  // Timer Interrupt
  var timerInterrupt: Boolean = false

  // Keyboard Interrupt - when used.
  var keyboardInterrupt: Boolean = false

  // Address of the last break we stopped at.
  var lastBreak: UInt = UInt(0)

  // Stop on a HALT
  def stopOnHALT: Boolean = {
    getBinaryOption("STOPONHALT")
  }


  def setClockFrquency(freq: Int): Unit = {
    clockFrequency = freq
    clockHasChanged = true
    Utils.outln(s"$getName: Clock frequency changed to: ${clockFrequency}Khz")
  }

  def runcpu(singleStep: Boolean = false): Unit // Main CPU execution loop

  def runcpu(singleStep:Boolean, startAddr: UInt)

  def onHalt(singleStepped: Boolean): Unit // called when CPU is about to be halted and returning to cmd line

  // Unit options common to all CPU's.
  override def createUnitOptions: Unit = {
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Stop on halt instruction.", value = false))

  }

  @inline
  final def setFlag(reg: Register8, flag: Int, clear: Boolean): Unit = {
    if (clear) reg(reg & ~flag) else reg(reg | flag)
  }

  @inline
  final def setFlag(reg: Register16, flag: Int, clear: Boolean): Unit = {
    if (clear) reg(reg & ~flag) else reg(reg | flag)
  }

  @inline
  final def testFlag(reg: Register8, flag: Int): Boolean = {
    if ((reg.intValue & flag) != 0) true else false
  }

  @inline
  final def testFlag(reg: Register16, flag: Int): Boolean = {
    if ((reg & flag) != 0) true else false
  }

  val registers: Map[String, Register]


  val MMU: BasicMMU

  private var MEMORYSIZE: UInt = UInt(0)

  def setMemorySize(size: UInt): Unit = {
    val maxsize = if (isBanked) MMU.MAXMEMORY else MMU.MAXBANKSIZE
    var newsize = size << KBLOG2
    if (isBanked) newsize = newsize & ~MMU.ADDRMASK
    if (newsize < KB) newsize = KB
    if (newsize > maxsize) newsize = maxsize
    if (newsize > size) newsize = size
    MEMORYSIZE = newsize
    awidth = MMU.MAXBANKSIZELOG2
    if (newsize > MMU.MAXBANKSIZE) awidth = awidth + MMU.MAXBANKSLOG2


    MMU.mapRAM(UInt(0x0000), MEMORYSIZE)

    Utils.outln(s"$getName: Memory size = ${Utils.formatBytes(MEMORYSIZE.toLong, false)} Banked: $isBanked")

  }

  def getMemorySize: UInt = MEMORYSIZE

  def resetCPU(): Unit

  /* UI Routines */
  def examine(address: Int, sb: StringBuilder): UByte = {
    val byte = MMU.get8(address)
    sb.append(f"$getName: 0x$address%04X:0x${byte.byteValue}%02X")
    byte
  }

  def examine(address: Int): UByte = {
    MMU.get8(address)

  }

  def examineWord(address: Int, sb: StringBuilder): UShort = {
    val word = MMU.get16(address)
    sb.append(f"$getName: 0x$address%04X:0x${word.shortValue}%04X")
    word
  }

  def examineWord(address: Int): UShort = {
    MMU.get16(address)
  }

  def examineRegister(nmemonic: String, sb: StringBuilder): Int = {
    registers.get(nmemonic) match {
      case Some(r: Register8) =>
        sb.append(f"$getName: $r")
        r.get8.intValue
      case Some(r: CompositeRegister16) =>
        sb.append(f"$getName: $r")
        r.get16.intValue
      case Some(r: Register16) =>
        sb.append(f"$getName: $r")
        r.get16.intValue
      case _ =>
        sb.append(s"$getName: Register $nmemonic is invalid.")
        0
    }
  }

  def deposit(address: Int, byte: UByte): Unit = {
    MMU.put8(address, byte)

  }

  def deposit(address: Int, byte: Int): Unit = {
    MMU.put8(address, UByte((byte & 0xff).byteValue()))
  }


  def depositWord(address: Int, word: UShort): Unit = {
    MMU.put16(address, word)
  }

  def depositWord(address: Int, word: Int): Unit = {
    MMU.put16(address, UShort((word & 0xffff).shortValue))
  }

  def showRegisters(): String

  def showFlags(): String

  def setRegister8(nmemonic: String, value: UByte): Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"$getName: Register $nmemonic is invalid.")
      case Some(r: Register8) =>
        r.set8(value)
      case _ =>
        Utils.outln(s"$getName: Register is not an 8 bit register.")
    }
  }

  def setRegister16(nmemonic: String, value: UShort): Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"$getName: Register $nmemonic is invalid.")
      case Some(r: Register16) =>
        r.set16(value)
      case _ =>
        Utils.outln(s"$getName: Register is not a 16 bit register.")
    }
  }


  override def showCommand(stringBuilder: StringBuilder): Unit = {
    super.showCommand(stringBuilder)
    stringBuilder.append(s"$getName: Registers:\n" + showRegisters() + "\n")
    stringBuilder.append(s"$getName: Flags:\n" + showFlags() + "\n")
  }

  // Dissassembly
  def DAsm(addr: Int, sb: StringBuilder): Int

  def DAsm(addr: Int, toAddr: Int, sb: StringBuilder): Int = {
    var pc = addr
    while (pc <= toAddr) {
      sb.append(f"\n$pc%08x: ")
      pc = DAsm(pc, sb)
    }
    pc
  }

  @inline
  // Memory log points.
  final def CHECK_LOG_BYTE(reg: Register16): Unit =  CHECK_LOG_BYTE(reg.get16)

  @inline // TODO Implement memory break points.
  final def CHECK_LOG_WORD(reg: Register16): Unit = CHECK_LOG_WORD(reg.get16)

  @inline
  final def CHECK_LOG_BYTE(v: Int): Unit = {
    if(machine.checkMemLog(UInt(v)) )Utils.outln(s"$getName: Memory Log Addr: ${v.toHexString} Value: ${MMU.get8(v).toHexString}")
  }

  @inline
  final def CHECK_LOG_WORD(addr: Int): Unit = {
    if(machine.checkMemLog(UInt(addr))) Utils.outln(s"$getName: Memory Log Addr: ${addr.toHexString} Value: ${MMU.get16(addr).toHexString}")
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
  def decrement(): Unit = value = UByte((value - UByte(1)).byteValue)

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
  override def get16: UShort = (low.get8 | (high.get8 << 8)).toUShort

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
  def set16(value: Int): Unit = this.value = UShort(value.shortValue())

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