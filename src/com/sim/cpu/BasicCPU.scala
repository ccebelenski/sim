package com.sim.cpu

import java.util.function.Consumer

import com.sim.{SimTimer, SimTimerUnit, Utils}
import com.sim.device.BasicDevice
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

  def runcpu(): Unit


  val registers: Map[String, Register]

  // Set up the master timer device
  SimTimer.sim_timer_init() // set up some universal stuff.
  val simTimerDevice = new SimTimer(machine)
  val masterTimer = new SimTimerUnit(simTimerDevice, true)
  SimTimer.internal_timer = masterTimer
  machine.devices.append(simTimerDevice)

  Utils.outln(s"SIM: OS Tick:${SimTimer.sim_os_tick_hz}Hz\tIdle Rate:${SimTimer.sim_idle_rate_ms}ms\tClock Res:${SimTimer.sim_os_clock_resolution_ms}ms")

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

    Utils.outln(s"Memory size = ${MEMORYSIZE.toHexString}")

  }

  def getMemorySize: UInt = MEMORYSIZE

  def resetCPU(): Unit

  /* UI Routines */
  def examine(address: UInt): Unit = {
    val byte = MMU.get8(address)
    Utils.outln(s"${address.toHexString}:${byte.toHexString}")
  }

  def examineWord(address: UInt): Unit = {
    val word = MMU.get16(address)
    Utils.outln(s"${address.toHexString}:${word.toHexString}")

  }

  def examineRegister(nmemonic: String): Unit = {
    registers.get(nmemonic) match {
      case Some(r: Register8) =>
        Utils.outln(s"SIM: ${r.get8.toHexString}")
      case Some(r: LittleEndianCompositeRegister16) =>
        Utils.outln(s"SIM: ${r.get16.toHexString}")
      case Some(r: Register16) =>
        Utils.outln(s"SIM: ${r.get16.toHexString}")

      case _ =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
    }
  }

  def deposit(address: UInt, byte: UByte): Unit = {
    MMU.put8(address, byte)

  }

  def depositWord(address: UInt, word: UShort): Unit = {
    MMU.put16(address, word)
  }

  def showRegisters(): Unit

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
  def set8(value: Register8) : Unit = set8(value.get8)

  override val aWidth = 8

  override def toString: String = s"$nmenomic:{$value.toHexString}"

}

// In the case of HL, H is most significant, L is least.  L would be written to memory first because Z80 is little endian
class LittleEndianCompositeRegister16(override val nmenomic: String, val msb: Register8, val lsb: Register8) extends Register16(nmenomic) {
  @inline
  def get8msb: UByte = msb.get8

  @inline
  def get8lsb: UByte = lsb.get8

  @inline
  override def get16: UShort = (lsb.get8 + (msb.get8 << 8)).toUShort

  @inline
  override def set16(value: UShort): Unit = {
    lsb.set8(UByte((value & 0xff).toByte))
    msb.set8(UByte(((value >> 8) & 0xFF).toByte))
  }

  @inline
  override def set16(value: Register16): Unit = {
    set16(value.get16)
  }

  @inline
  def set8msb(value: UByte): Unit = {
    msb.set8(value)
  }

  @inline
  def set8lsb(value: UByte): Unit = {
    lsb.set8(value)
  }

  override val aWidth = 16

  override def toString: String = s"${msb.nmenomic}${lsb.nmenomic}:${get16.toHexString}"
}

class Register16(override val nmenomic: String) extends Register(nmenomic) {
  private var value: UShort = UShort(0)

  @inline
  def get16: UShort = value

  @inline
  def set16(value: UShort): Unit = this.value = value

  @inline
  def set16(value: Register16): Unit = this.value = value.get16

  override val aWidth = 16

  override def toString: String = s"$nmenomic:{$value.toHexString}"
}