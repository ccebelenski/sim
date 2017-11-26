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
  }


  def logCapacity(): Unit = {
  }

  def isAvailable: Boolean = {
  true
  }

  def isWriteProtected: Boolean = {
    writeLock
  }

  def getDiskSize: ULong = {
ULong(0)
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
