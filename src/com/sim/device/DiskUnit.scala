package com.sim.device

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path}
import java.util.Objects

import com.sim.Utils
import com.sim.unsigned.UInt
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by christophercebelenski on 7/18/16.
  */
trait DiskUnit extends BasicUnit with UnitAttachable with SupportsOptions{

  val logger: Logger = LoggerFactory.getLogger(classOf[DiskUnit])

  // The user controlled options come from the options...


  // Java filechannel
  var fileChannel : FileChannel = _
  var byteBuffer: ByteBuffer = _

  // disk sector size
  var sectorSize: Int = 512
  // Units of capacity( word, byte)
  var capacFactor: DiskCapacityFormat.Value = DiskCapacityFormat.Byte
  // Sector size of the containing storage
  var storageSectorSize: UInt = UInt(0)
  var autoFormat: Boolean = false

  var FixedCapacity: Boolean = false
  var isSequential: Boolean = false
  var isIdleEligible: Boolean = true

  // Capacity
  var capacity: Long = 0L

  // File position
  var pos: Long = 0L

  // I/O Start time
  var ioStartTime: Long = 0L

  var dirty:Boolean = false

  def readSector()

  def writeSector()

  def seek(pos:Long)

  /**
    * Quick check for write-protect
    * @return
    */
  def isWriteProtect: Boolean = {
    getBinaryOption("READONLY")
  }

  /**
    * Disk format - should be SIMH or VHD
    * @return
    */
  def getDiskFormat: String = {

    getEnumValueOption("FORMAT").getOrElse("UNDEFINED")
  }

  /**
    * Checks to see if the disk is avail...
    *
    * @return
    */
  def isAvailable:Boolean = {
    if(attachedPath.isEmpty) return false
    if(capacity <= 0) return false


    true
  }

  def setCapacity(cap:String, sb:StringBuilder) : Boolean = {

    Objects.requireNonNull(cap)
    val bytes:Long  = Utils.toBytes(cap)
    if(bytes <= 0 || bytes >= 20000000) {

      sb.append(s"$getName: Disk Unit misconfiguration - Capacity out of spec.")
      return false
    } else  if(sectorSize <= 0) {
      sb.append(s"$getName: Disk Unit misconfiguration - Sector size out of spec.")
      return false
    }
      capacity = ((bytes * 1000000) / sectorSize).longValue()
      sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity,true)} sectors.")

      true
  }

  def showCapacity(sb:StringBuilder) : Unit = {
    val cap = capacity * sectorSize
    val capUnits = {if(capacFactor == DiskCapacityFormat.Byte) "B" else "W"}
    if(cap == 0) {
      sb.append(s"$getName: Capacity Undefined.")
    } else {
      if(cap >= 1000000) sb.append(s"$getName: Capacity: ${cap/1000000}M$capUnits")
      else if(cap >= 1000) sb.append(s"$getName: Capacity: ${cap/1000}K$capUnits")
      else sb.append(s"$getName: Capacity: ${cap}$capUnits")
    }
  }

  /**
    * get disk size - #450 sim_disk.c
    * Assumes disk is attached...
    * @return
    */
  protected def sim_disk_size:Long = {
    var physical_size:Long = 0L
    var filesystem_size:Long = get_filesystem_size()

    physical_size = getDiskFormat match {
      case "SIMH" => sim_fsize_ex(attachedPath.get)
      case "VHD" => sim_vhd_disk_size()
      case _ => -1
    }
    if(filesystem_size == -1 || filesystem_size < physical_size) physical_size else filesystem_size
  }

  private def sim_fsize_ex(p:Path) : Long = {
    Files.size(p)
  }

  // TODO VHD Not implmented here.
  private def sim_vhd_disk_size(): Long = {-1L}
  // TODO ODS1,ODS2,Ultrix filesystems...
  private def get_filesystem_size() : Long = {-1L}


  override def attach(sectorSize: Int,
                      xferElementSize: Int,
                      autosize: Boolean,
                      driveType: String,
                      pdp11Tracksize: Int,
                      completetionDelay: Int) : StringBuilder = {
    val sb:StringBuilder = new StringBuilder
    



    sb
  }

}


object DiskCapacityFormat extends Enumeration {
  val Byte, Word = Value
}

class DiskUnitAttachedException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {
}

class UnsupportedDiskFormatException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {

}
