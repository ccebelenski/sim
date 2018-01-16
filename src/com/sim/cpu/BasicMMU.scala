package com.sim.cpu

import com.sim.memory.{AddressSpace, MemoryAddressSpace}
import com.sim.Console
import com.sim.device.{MemoryMappedUnit, PortMappedUnit}
import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/18/16.
  *
  * A basic MMU that supports simple bank switching.
  *
  */
abstract class BasicMMU(val cpu: BasicCPU) {

  val tt = Console.textTerminal

  // Some constants - allow for overrides later

  val MAXBANKSIZE : UInt= UInt(65536) // Max memory size, power of 2
  val MAXBANKSIZELOG2 : UInt  = UInt((Math.log(MAXBANKSIZE.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKSIZE
  val MAXBANKS :UInt = UInt(16) // Max number of memory banks, a power of 2
  val MAXBANKSLOG2 : UInt = UInt((Math.log(MAXBANKS.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKS
  val MAXMEMORY :UInt = MAXBANKS * MAXBANKSIZE  // Maximum total memory size
  val ADDRMASK :UInt = MAXBANKSIZE - UInt(1) // Address mask
  val ADDRMASKEXTENDED :UInt = MAXMEMORY - UInt(1)
  val BANKMASK :UInt = MAXBANKS - UInt(1)

  val LOG2PAGESIZE : UInt = UInt(8)
  val PAGESIZE : UInt = UInt(1) <<  LOG2PAGESIZE

  val COMMON = 0xc000 // Addreses greater than common are in the same memory.

  val mtab : Array[Option[MMU_ENTRY]] = new Array[Option[MMU_ENTRY]]((MAXMEMORY >> LOG2PAGESIZE).toInt)
  for(x <- mtab.indices) mtab(x) = None

  // 256 Ports
  val iotab : Array[Option[MMU_ENTRY]] = new Array[Option[MMU_ENTRY]](256)

  private var bankSelect: Int = 0


  def mapRAM(baseAddress: UInt, size: UInt) : Unit = {

    for(i <- 0 to (size >> LOG2PAGESIZE).toInt) {
      var addr = (baseAddress & 0xfff00 ) + (i << LOG2PAGESIZE.toInt)
      if(cpu.isBanked && addr < COMMON ) addr = addr | (bankSelect << MAXBANKSIZELOG2.toInt)
      val page = addr >> LOG2PAGESIZE.toInt
      val as = new MemoryAddressSpace(UInt(addr), UInt(addr) + LOG2PAGESIZE)
      val entry = MMU_ENTRY(memory = Some(as))
      mtab(page) = Some(entry)
      tt.println(s"MMU: Mapped RAM memory space.  Page: $page, Addr: $addr - ${as.highAddress}")

    }

  }

  def mapMemoryMappedUnit(u: MemoryMappedUnit) : Unit = {

    // TODO
  }

  def mapPortMappedUnit(u: PortMappedUnit) : Unit = {

    for(i <- u.port to u.port + u.size ) {
      if(iotab(i & 0xff).isDefined) {
        tt.println(s"MMU: IO Port: ${i & 0xff} already mapped to Unit: ${iotab(i & 0xff).get.portUnit.get.unitName}")
      } else {
        iotab(i & 0xff) = Some(MMU_ENTRY(portUnit = Some(u)))
        tt.println(s"MMU: Mapping IO Port: ${i & 0xff} Unit: ${u.unitName}")
      }
    }
  }

  def unMapIOPort(u: PortMappedUnit): Unit = {

    for(i <- u.port to u.port + u.size ) {
      if(iotab(i & 0xff).isDefined) {
        tt.println(s"MMU: Unmapping IO Port: ${i & 0xff} Unit: ${u.unitName}")
        iotab(i & 0xff) = None
      } else {
        tt.println(s"MMU: IO Port: ${i & 0xff} is not mapped.")
      }
    }
  }

  def printAddressSpace() : Unit = {

    for(x <- mtab.indices) {
      val e = mtab(x)
      if(e.isDefined) {
        if (e.get.memory.isDefined) {
          val as = e.get.memory.get
          tt.println(s"Page: $x : ${as.lowAddress.toHexString} - ${as.highAddress.toHexString} : Type: ${as.getClass} Mapped: RO: ${as.isReadOnly}")
        } else if (e.get.memoryUnit.isDefined) {
          val unit = e.get.memoryUnit.get
          tt.println(s"Page: $x : ${unit.lowAddress.toHexString} - ${unit.highAddress.toHexString} : Type: ${unit.getClass}")
        }
      }

    }

  }


  def put8(address: UInt, value: UInt) : Unit = {

  }


  def selectBank(bank:Int) : Unit = this.bankSelect = bank
  def getBank: Int = this.bankSelect
}
case class MMU_ENTRY(memoryUnit: Option[MemoryMappedUnit] = None, portUnit : Option[PortMappedUnit] = None, memory: Option[AddressSpace] = None)