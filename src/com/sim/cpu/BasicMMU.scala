package com.sim.cpu

import com.sim.Utils
import com.sim.device.{MemoryMappedDevice, PortMappedDevice}
import com.sim.memory.{AddressSpace, MemoryAddressSpace, ROMAddressSpace}
import com.sim.unsigned.{UByte, UInt, UShort}

/**
  * Created by christophercebelenski on 7/18/16.
  *
  * A basic MMU that supports simple bank switching.
  *
  */
abstract class BasicMMU(val cpu: BasicCPU) {

  // Some constants - allow for overrides later

  val MAXBANKSIZE: UInt = UInt(65536) // Max memory size, power of 2
  val MAXBANKSIZELOG2: UInt = UInt((Math.log(MAXBANKSIZE.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKSIZE
  val MAXBANKS: UInt = UInt(16) // Max number of memory banks, a power of 2
  val MAXBANKSLOG2: UInt = UInt((Math.log(MAXBANKS.toInt) / Math.log(2)).intValue()) // log2 of MAXBANKS
  val MAXMEMORY: UInt = MAXBANKS * MAXBANKSIZE // Maximum total memory size
  val ADDRMASK: UInt = MAXMEMORY - UInt(1) // Address mask
  val ADDRMASKEXTENDED: UInt = MAXMEMORY - UInt(1)
  val BANKMASK: UInt = MAXBANKS - UInt(1)

  val LOG2PAGESIZE: UInt = UInt(8)
  val PAGESIZE: UInt = UInt(1) << LOG2PAGESIZE

  val COMMON = 0xc000 // Addreses greater than common are in the same memory.

  val mtab: Array[Option[MMU_ENTRY]] = new Array[Option[MMU_ENTRY]](1+(MAXMEMORY >> LOG2PAGESIZE).toInt)
  for (x <- mtab.indices) mtab(x) = None

  // 256 Ports
  val iotab: Array[Option[PortMappedDevice]] = new Array[Option[PortMappedDevice]](256)
  for (x <- iotab.indices) iotab(x) = None

  private var bankSelect: Int = 0


  def mapRAM(baseAddress: UInt, size: UInt): Unit = {

    for (i <- 0 to (size >> LOG2PAGESIZE).toInt) {
      var addr = (baseAddress & 0xfff00) + (i << LOG2PAGESIZE.toInt)
      val pageaddr = if (cpu.isBanked && addr < COMMON) addr | (bankSelect << MAXBANKSIZELOG2.toInt) else addr
      val page = pageaddr >> LOG2PAGESIZE.toInt
      val as = new MemoryAddressSpace(UInt(addr), UInt(addr) + PAGESIZE - UInt(1))
      val entry = MMU_ENTRY(memory = Some(as))
      mtab(page) = Some(entry)
      Utils.outln(f"MMU: Mapped RAM memory space.  Page: 0x$page%04X, Addr: 0x$addr%04X - 0x${as.highAddress.intValue}%04X")
    }

  }

  def mapROM(baseAddress:UInt, size:UInt, image:Array[Int]): Unit = {
    for (i <- 0 to (size >> LOG2PAGESIZE).toInt) {
      var addr = (baseAddress & 0xfff00) + (i << LOG2PAGESIZE.toInt)
      val pageaddr = if (cpu.isBanked && addr < COMMON) addr | (bankSelect << MAXBANKSIZELOG2.toInt) else addr
      val page = pageaddr >> LOG2PAGESIZE.toInt
      val as = new ROMAddressSpace(UInt(addr), UInt(addr) + PAGESIZE - UInt(1))

      val imgIdx = i << LOG2PAGESIZE.toInt
      val imgSize = PAGESIZE - UInt(1)
      for(x <- imgIdx to imgSize.toInt) as.load8(UInt(addr + x), UByte(image(x).byteValue()))

      val entry = MMU_ENTRY(memory = Some(as))
      mtab(page) = Some(entry)
      Utils.outln(f"MMU: Mapped ROM memory space.  Page: 0x$page%04X, Addr: 0x$addr%04X - 0x${as.highAddress.intValue}%04X")
    }


  }

  def mapMemoryMappedDevice(u: MemoryMappedDevice): Unit = {

    // TODO
  }

  def mapPortMappedDevice(u: PortMappedDevice): Unit = {

    u.ports.foreach(i => {
      if (iotab(i & 0xff).isDefined) {
        Utils.outln(f"MMU: IO Port: 0x${i & 0xff}%02X already mapped to Unit: ${iotab(i & 0xff).get.getName}")
      } else {
        iotab(i & 0xff) = Some(u)
        Utils.outln(f"MMU: Mapping IO Port: 0x${i & 0xff}%02X Device: ${u.getName}")
      }
    })
  }

  def unMapIOPort(u: PortMappedDevice): Unit = {

    u.ports.foreach(i => {
      if (iotab(i & 0xff).isDefined) {
        Utils.outln(f"MMU: Unmapping IO Port: 0x${i & 0xff}%02X Device: ${u.getName}")
        iotab(i & 0xff) = None
      } else {
        Utils.outln(f"MMU: IO Port: 0x${i & 0xff}%02X is not mapped.")
      }
    })
  }

  def printAddressSpace(): Unit = {

    for (x <- mtab.indices) {
      val e = mtab(x)
      if (e.isDefined) {
        if (e.get.memory.isDefined) {
          val as = e.get.memory.get
          val msg = f"Page: 0x$x%04X : 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X : Type: ${as.getClass} Mapped: RO: ${as.isReadOnly}"
          Utils.outln(msg)
        } else if (e.get.memoryDevice.isDefined) {
          val unit = e.get.memoryDevice.get
          val msg = f"Page: 0x$x%04X : 0x${unit.lowAddress.intValue}%04X - 0x${unit.highAddress.intValue}%04X : Type: ${unit.getClass}"
          Utils.outln(msg)
        }
      }

    }

  }

  def installROM(image:Array[Int], size:Int, baseAddr:UInt) : Unit = {
    mapROM(baseAddr,UInt(image.size - 1),image)
  }


  @inline
  def put8(register16: Register16, value: Register8): Unit = {
    put8(register16.get16.toInt, value.get8.toUByte)
  }

  @inline
  def put8(register16: Register16, value: UByte): Unit = {
    put8(register16.get16.toInt, value)
  }

  @inline
  def put8(register16:Register16, value: Int) : Unit = {
    put8(register16.get16,UByte(value.byteValue()))
  }

  @inline
  def put8(address: UShort, value: Register8): Unit = {
    put8(address.intValue, value.get8)
  }

  def put8(address: Int, value: UByte): Unit = {

    var addr: Int = address & ADDRMASK
    val pageaddr = if (cpu.isBanked && (addr < COMMON)) addr | bankSelect << MAXBANKSIZELOG2.toInt else addr
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None => Utils.outln(f"MMU: Write to non-existent memory.  Addr: 0x$addr%04X")
      case Some(e: MMU_ENTRY) => {
        if (e.memory.isDefined) {
          val as = e.memory.get
          if (as.containsAddress(UInt(addr))) as.put8(UInt(addr), value)
          else {
            val msg = f"MMU: Page table error - Addr: 0x$addr%04X address space: 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X"
            Utils.outln(msg)
          }
        } else if (e.memoryDevice.isDefined) {
          // TODO This needs work
          e.memoryDevice.get.action(UInt(addr), value, isWrite = true)
        } else Utils.outln(f"MMU: Mis-configured page/address entry - no type found. Addr: 0x$addr%04X")
      }
    }

  }

  def out8(address: Int, value: UByte): Unit = {
    iotab(address & 0xff) match {
      case None => Utils.outln(s"MMU: Write to unconnected port.  Port:${address & 0xff}")
      case Some(pmu) => pmu.action(UInt(address), value, isWrite = true)
    }

  }

  def out8(r1:Register8, value: UByte) : Unit = {
    out8(r1.intValue,value)
  }

  def out8(r1:Register8, value:Int) : Unit = {
    out8(r1.intValue, UByte(value.byteValue()))
  }

  def in8(address: Int): UByte = {
    iotab(address & 0xff) match {
      case None => {
        Utils.outln(s"MMU: Read from unconnected port.  Port:${address & 0xff}")
        UByte(0)
      }
      case Some(pmu) => pmu.action(UInt(address), UByte(0), isWrite = false)
    }
  }

  // Store little endian...
  def put16(address: Int, value: UShort): Unit = {

    put8(address, UByte((value & 0xFF).toByte))
    put8(address + 1, UByte(((value >> 8) & 0xFF).toByte))
  }

  def get8(address: Register16): UByte = {
    get8(address.get16)
  }

  def get8(address: Int): UByte = {
    var addr: Int = (address & ADDRMASK).toInt
    val pageaddr = if (cpu.isBanked && (addr < COMMON)) addr | bankSelect << MAXBANKSIZELOG2.toInt else addr
    val m = mtab(pageaddr >> LOG2PAGESIZE.toInt)
    m match {
      case None =>
        Utils.outln(f"MMU: Read from non-existent memory.  Addr: $addr%04X")
        UByte(0)
      case Some(e: MMU_ENTRY) => {
        if (e.memory.isDefined) {
          val as = e.memory.get
          if (as.containsAddress(UInt(addr))) e.memory.get.get8(UInt(addr))
          else {
            val msg = f"MMU: Page table error - Addr: 0x$addr%04X address space: 0x${as.lowAddress.intValue}%04X - 0x${as.highAddress.intValue}%04X"
            Utils.outln(msg)
            UByte(0)
          }
        } else if (e.memoryDevice.isDefined) {
          // TODO This needs work.
          e.memoryDevice.get.action(UInt(addr), UByte(0), isWrite = false)
        } else {
          Utils.outln(f"MMU: Mis-configured page/address entry - no type found. Addr: 0x$addr%04X")
          UByte(0)
        }
      }
    }
  }


  // Retrieve little endian
  @inline
  def get16(address: Int): UShort = {
    UShort((get8(address) | (get8(address + UInt(1)) << 8)).toShort)
  }

  @inline
  def get16(register16: Register16): UShort = {
    get16(register16.get16)
  }


  @inline
  def get16(address:UShort) : UShort = {
    get16(address.intValue)
  }

  def selectBank(bank: Int): Unit = this.bankSelect = bank

  def getBank: Int = this.bankSelect
}

case class MMU_ENTRY(memoryDevice: Option[MemoryMappedDevice] = None, memory: Option[AddressSpace] = None)
