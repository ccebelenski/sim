package com.sim.cpu

import com.sim.device.BasicDevice
import com.sim.unsigned.UInt

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

  }

  def getMemorySize: UInt = MEMORYSIZE

}
