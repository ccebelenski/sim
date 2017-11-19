package com.sim.device

import java.nio.file.Files

import com.sim.Utils
import com.sim.unsigned.{UInt, ULong}
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by christophercebelenski on 7/18/16.
  */
trait DiskUnit extends BasicUnit {

  val logger: Logger = LoggerFactory.getLogger(classOf[DiskUnit])

  var device: DiskDevice = _
  // debugging bit
  var dbit: UInt = UInt(0)
  // disk sector size
  var sectorSize: UInt = UInt(0)
  // Units of capacity( word, byte)
  var capacFactor: DiskCapacityFormat.Value = DiskCapacityFormat.Byte
  // Sector size of the containing storage
  var storageSectorSize: UInt = UInt(0)
  var removable: Boolean = false
  var autoFormat: Boolean = false

  var writeLock: Boolean = false

  var format: DiskFormatValue.Value = DiskFormatValue.Standard

  def attach(sectorSize: Int, xferElementSize: Int, autosize: Boolean, driveType: String, pdp11Tracksize: Int, completetionDelay: Int)

  def detach()

  def rdsect(lba: Int, buf: DiskBuffer)

  def setFormat(value: DiskFormatValue.Value): Unit = {
    this.format = value
  }

  def setCapacity(capacity: String): Unit = {
    if (attached) throw new DiskUnitAttachedException(this, "Unit Already Attached.")

    val cap = Utils.getUint(capacity, UInt(10), UInt(2000000))

    val div = if ((device.flags & BasicDevice.DEV_SECTORS) != 0) UInt(512) else UInt(1)
    capac = (cap * UInt(1000000)) / div

  }


  def logCapacity(): Unit = {
    val div = if ((device.flags & BasicDevice.DEV_SECTORS) != 0) UInt(512) else UInt(1)
    val capac = this.capac * div
    val cap_uints = if (device.dwidth / device.aincr == 16) "W" else "B"
    val cstring = if (capac != 0) {
      if (capac >= UInt(1000000)) "capacity=%dM%s".format((capac / UInt(1000000)).intValue, cap_uints)
      else if (capac >= UInt(1000)) "capacity=%dK%s".format((capac / UInt(1000)).intValue, cap_uints)
      else "capacity=%d%s".format(capac.intValue, cap_uints)
    } else "undefined capacity"

    logger.info(cstring)
  }

  def isAvailable: Boolean = {
    attached
  }

  def isWriteProtected: Boolean = {
    writeLock
  }

  def getDiskSize: ULong = {

    if (format == DiskFormatValue.Standard)
      ULong(Files.size(path))

    else throw new UnsupportedDiskFormatException(this, "Only standard format implemented.")
  }

}

class DiskBuffer {

}

case class DiskFormat(name: String, uflags: UInt, fmttype: DiskFormatValue.Value, fmtval: UInt)

object DiskFormatValue extends Enumeration {
  val Standard, Raw, VHD = Value
}

object DiskCapacityFormat extends Enumeration {
  val Byte, Word = Value
}

class DiskUnitAttachedException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {
}

class UnsupportedDiskFormatException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {

}
