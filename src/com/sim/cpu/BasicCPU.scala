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

  def examine(address: UInt) : Unit = {
    val byte = MMU.get8(address)
    Utils.outln(s"${address.toHexString}:${byte.toHexString}")
  }
  def examineWord(address:UInt) : Unit = {
    val word = MMU.get16(address)
    Utils.outln(s"${address.toHexString}:${word.toHexString}")

  }
  def deposit(address: UInt, byte:UByte) : Unit = {
    MMU.put8(address,byte)

  }
  def depositWord(address:UInt, word:UShort) : Unit = {
    MMU.put16(address,word)
  }

}
