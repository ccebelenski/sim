package com.sim.cpu

import com.sim.Utils
import com.sim.device.BasicDevice
import com.sim.unsigned.{UByte, UInt, UShort}

/**
  * Created by christophercebelenski on 7/1/16.
  */
abstract class BasicCPU(val isBanked: Boolean) extends BasicDevice {

  val KBLOG2 = UInt(10)
  val KB = UInt(1024)



  val registers: Map[String,Register]


  val MMU: BasicMMU

  private var MEMORYSIZE : UInt = UInt(0)

  def setMemorySize(size: UInt) : Unit = {
    val maxsize = if(isBanked) MMU.MAXMEMORY else MMU.MAXBANKSIZE
    var newsize = size << KBLOG2
    if(isBanked) newsize = newsize & ~MMU.ADDRMASK
    if(newsize < KB) newsize = KB
    if(newsize > maxsize) newsize = maxsize
    MEMORYSIZE = newsize
    awidth = MMU.MAXBANKSIZELOG2
    if(newsize > MMU.MAXBANKSIZE) awidth = awidth + MMU.MAXBANKSLOG2

    Utils.outln(s"Memory size = ${MEMORYSIZE.toHexString}")
  }

  def getMemorySize: UInt = MEMORYSIZE

  def resetCPU() : Unit

  /* UI Routines */
  def examine(address: UInt) : Unit = {
    val byte = MMU.get8(address)
    Utils.outln(s"${address.toHexString}:${byte.toHexString}")
  }
  def examineWord(address:UInt) : Unit = {
    val word = MMU.get16(address)
    Utils.outln(s"${address.toHexString}:${word.toHexString}")

  }
  def examineRegister(nmemonic:String) : Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
      case Some(r:Register8) =>
        Utils.outln(s"SIM: ${r.get8.toHexString}")
      case Some(r:Register16) =>
        Utils.outln(s"SIM: ${r.get16.toHexString}")
      case Some(r:LittleEndianCompositeRegister16) =>
        Utils.outln(s"SIM: ${r.get16.toHexString}")
    }
  }

  def deposit(address: UInt, byte:UByte) : Unit = {
    MMU.put8(address,byte)

  }
  def depositWord(address:UInt, word:UShort) : Unit = {
    MMU.put16(address,word)
  }

  def showRegisters(): Unit
  def setRegister8(nmemonic : String, value : UByte) : Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
      case Some(r:Register8) =>
        r.set8(value)
      case _ =>
        Utils.outln(s"SIM: Register is not an 8 bit register.")
    }
  }
  def setRegister16(nmemonic : String, value: UShort) : Unit = {
    registers.get(nmemonic) match {
      case None =>
        Utils.outln(s"SIM: Register $nmemonic is invalid.")
      case Some(r:Register16) =>
        r.set16(value)
      case _ =>
        Utils.outln(s"SIM: Register is not a 16 bit register.")
    }
  }

}

abstract class Register(val nmenomic:String) {
  val aWidth: Int
}

class Register8(override val nmenomic:String) extends Register(nmenomic) {
  private var value: UByte = UByte(0)
  def get8 = value
  def set8(value:UByte) = this.value = value
  override val aWidth = 8

  override def toString: String = s"$nmenomic:{$value.toHexString}"

}
// In the case of HL, H is most significant, L is least.  L would be written to memory first because Z80 is little endian
class LittleEndianCompositeRegister16(override val nmenomic:String, val msb: Register8, val lsb: Register8) extends Register16(nmenomic) {
  def get8msb : UByte = msb.get8
  def get8lsb : UByte = lsb.get8
  override def get16 :UShort = (lsb.get8 + (msb.get8 << 8)).toUShort
  override def set16(value:UShort): Unit  = {
    lsb.set8(UByte((value & 0xff).toByte))
    msb.set8(UByte(((value >> 8) & 0xFF).toByte))
  }
  def set8msb(value:UByte): Unit = {msb.set8(value)}
  def set8lsb(value:UByte): Unit = {lsb.set8(value)}

  override val aWidth = 16
  override def toString: String = s"${msb.nmenomic}${lsb.nmenomic}:${get16.toHexString}"
}

class Register16(override val nmenomic:String) extends Register(nmenomic) {
  private var value: UShort = UShort(0)
  def get16: UShort = value
  def set16(value:UShort):Unit  = this.value = value

  override val aWidth = 16
  override def toString: String = s"$nmenomic:{$value.toHexString}"
}