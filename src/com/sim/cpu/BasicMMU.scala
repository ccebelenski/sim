package com.sim.cpu

import com.sim.Utils
import com.sim.device.{MemoryMappedUnit, PortMappedUnit}
import com.sim.memory.{AddressSpace, MemoryAddressSpace}
import com.sim.unsigned.{UByte, UInt, UShort}

/**
  * Created by christophercebelenski on 7/18/16.
  *
  * A basic MMU that supports simple bank switching.
  *
  */
abstract class BasicMMU(val cpu: BasicCPU) {

  // Some constants - allow for overrides later

  val MAXBANKSIZE : UInt= UInt(65536) // Max memory size, power of 2
  val MAXBANKSIZELOG2 : UInt  = UInt((Math.log(MAXBANKSIZE.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKSIZE
  val MAXBANKS :UInt = UInt(16) // Max number of memory banks, a power of 2
  val MAXBANKSLOG2 : UInt = UInt((Math.log(MAXBANKS.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKS
  val MAXMEMORY :UInt = MAXBANKS * MAXBANKSIZE  // Maximum total memory size
  val ADDRMASK :UInt = MAXMEMORY - UInt(1) // Address mask
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
      val pageaddr = if(cpu.isBanked && addr < COMMON ) addr | (bankSelect << MAXBANKSIZELOG2.toInt) else addr
      val page = pageaddr >> LOG2PAGESIZE.toInt
      val as = new MemoryAddressSpace(UInt(addr), UInt(addr) + PAGESIZE - UInt(1))
      val entry = MMU_ENTRY(memory = Some(as))
      mtab(page) = Some(entry)
      Utils.outln(s"MMU: Mapped RAM memory space.  Page: ${page.toHexString}, Addr: ${addr.toHexString} - ${as.highAddress.toHexString}")
    }

  }

  def mapMemoryMappedUnit(u: MemoryMappedUnit) : Unit = {

    // TODO
  }

  def mapPortMappedUnit(u: PortMappedUnit) : Unit = {

    for(i <- u.port to u.port + u.size ) {
      if(iotab(i & 0xff).isDefined) {
        Utils.outln(s"MMU: IO Port: ${i & 0xff} already mapped to Unit: ${iotab(i & 0xff).get.portUnit.get.getName()}")
      } else {
        iotab(i & 0xff) = Some(MMU_ENTRY(portUnit = Some(u)))
        Utils.outln(s"MMU: Mapping IO Port: ${i & 0xff} Unit: ${u.getName()}")
      }
    }
  }

  def unMapIOPort(u: PortMappedUnit): Unit = {

    for(i <- u.port to u.port + u.size ) {
      if(iotab(i & 0xff).isDefined) {
        Utils.outln(s"MMU: Unmapping IO Port: ${i & 0xff} Unit: ${u.getName()}")
        iotab(i & 0xff) = None
      } else {
        Utils.outln(s"MMU: IO Port: ${i & 0xff} is not mapped.")
      }
    }
  }

  def printAddressSpace() : Unit = {

    for(x <- mtab.indices) {
      val e = mtab(x)
      if(e.isDefined) {
        if (e.get.memory.isDefined) {
          val as = e.get.memory.get
          val msg = s"Page: ${x.toHexString} : ${as.lowAddress.toHexString} - ${as.highAddress.toHexString} : Type: ${as.getClass} Mapped: RO: ${as.isReadOnly}"
          Utils.outln(msg)
        } else if (e.get.memoryUnit.isDefined) {
          val unit = e.get.memoryUnit.get
          val msg = s"Page: ${x.toHexString} : ${unit.lowAddress.toHexString} - ${unit.highAddress.toHexString} : Type: ${unit.getClass}"
          Utils.outln(msg)
        }
      }

    }

  }


  def put8(address: UInt, value: UByte) : Unit = {

    var addr: Int = (address & ADDRMASK).toInt
    val pageaddr = if(cpu.isBanked && (addr < COMMON)) addr | bankSelect << MAXBANKSIZELOG2.toInt else addr
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None => Utils.outln(s"MMU: Write to non-existent memory.  Addr: ${addr.toHexString}")
      case Some(e: MMU_ENTRY) => {
        if(e.memory.isDefined) {
          val as = e.memory.get
          if(as.containsAddress(UInt(addr))) e.memory.get.put8(UInt(addr),value)
          else {
            val msg = s"MMU: Page table error - Addr: ${addr.toHexString} address space: ${as.lowAddress.toHexString} - ${as.highAddress.toHexString}"
            Utils.outln(msg)
          }
        }else if(e.portUnit.isDefined) {
          // TODO - This should not map here.
        } else if(e.memoryUnit.isDefined) {
          // TODO This needs work
          e.memoryUnit.get.action(UInt(addr),value, isWrite = true)
        } else Utils.outln(s"MMU: Mis-configured page/address entry - no type found. Addr: ${addr.toHexString}")
      }
    }

  }

  // Store little endian...
  def put16(address: UInt, value: UShort) : Unit = {

    put8(address + UInt(1), UByte((value & 0xFF).toByte))
    put8(address, UByte(((value >> 8) & 0xFF).toByte))
  }

  def get8(address: UInt) : UByte = {
    var addr: Int = (address & ADDRMASK).toInt
    val pageaddr = if(cpu.isBanked && (addr < COMMON)) addr | bankSelect << MAXBANKSIZELOG2.toInt else addr
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None =>
        Utils.outln(s"MMU: Read from non-existent memory.  Addr: ${addr.toHexString}")
        UByte(0)
      case Some(e: MMU_ENTRY) => {
        if(e.memory.isDefined) {
          val as = e.memory.get
          if(as.containsAddress(UInt(addr))) e.memory.get.get8(UInt(addr))
          else {
            val msg = s"MMU: Page table error - Addr: ${addr.toHexString} address space: ${as.lowAddress.toHexString} - ${as.highAddress.toHexString}"
            Utils.outln(msg)
            UByte(0)
          }
        }else if(e.portUnit.isDefined) {
          // TODO - This should not map here.
          UByte(0)
        } else if(e.memoryUnit.isDefined) {
          // TODO This needs work.
          e.memoryUnit.get.action(UInt(addr),UByte(0),isWrite = false)
        } else {
          Utils.outln(s"MMU: Mis-configured page/address entry - no type found. Addr: ${addr.toHexString}")
          UByte(0)
        }
      }
    }
  }

  // Retrieve little endian
  def get16(address: UInt): UShort = {
    UShort((get8(address + UInt(1)) + (get8(address) << 8)).toShort)
  }

  def selectBank(bank:Int) : Unit = this.bankSelect = bank
  def getBank: Int = this.bankSelect
}
case class MMU_ENTRY(memoryUnit: Option[MemoryMappedUnit] = None, portUnit : Option[PortMappedUnit] = None, memory: Option[AddressSpace] = None)
