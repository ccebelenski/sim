package com.sim.device

import java.nio._
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption._
import java.nio.file.{Files, OpenOption, Path, Paths}
import java.util

import com.sim.Utils
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by christophercebelenski on 7/18/16.
  */
trait DiskUnit extends BasicUnit with UnitAttachable with SupportsOptions {

  val logger: Logger = LoggerFactory.getLogger(classOf[DiskUnit])

  // The user controlled options come from the options...


  // Java filechannel
  var fileChannel: FileChannel = _
  var byteBuffer: ByteBuffer = _

  // These should be overridden
  val MAX_TRACKS: Int = 0
  val DSK_SECT = 0
  val DSK_SECTSIZE = 0

  // Unit specific information
  var current_track: Int = 0
  var current_sector: Int = 0
  var current_byte: Int = 0

  var sectors_per_track: Int = DSK_SECT
  var tracks: Int = MAX_TRACKS


  var FixedCapacity: Boolean = false
  var isSequential: Boolean = false
  var isIdleEligible: Boolean = true

  // Capacity
  var capacity: Long = 0L

  // I/O Start time
  var ioStartTime: Long = 0L

  var dirty: Boolean = false

  def readSector(): Unit = {
    byteBuffer.clear()
    fileChannel.position(DSK_SECTSIZE * sectors_per_track * current_track +
      DSK_SECTSIZE * current_sector)
    do {
      fileChannel.read(byteBuffer)
    } while (byteBuffer.hasRemaining)

    current_byte = 0
  }

  def seek(): Unit = {
    fileChannel.position(DSK_SECTSIZE * sectors_per_track * current_track +
      DSK_SECTSIZE * current_sector)
  }

  /**
    * Quick check for write-protect
    *
    * @return
    */
  def isWriteProtect: Boolean = {
    getBinaryOption("READONLY")
  }

  /**
    * Disk format - should be SIMH or VHD
    *
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
  def isAvailable: Boolean = {
    if (attachedPath.isEmpty) return false
    if (capacity <= 0) return false


    true
  }


  /**
    * get disk size - #450 sim_disk.c
    * Assumes disk is attached...
    *
    * @return
    */
  protected def sim_disk_size: Long = {
    var physical_size: Long = 0L

    physical_size = getDiskFormat match {
      case "SIMH" => sim_fsize_ex(attachedPath.get)
      case _ => -1
    }
    physical_size
  }

  private def sim_fsize_ex(p: Path): Long = {
    Files.size(p)
  }


  override def attach(fileSpec: String, sb: StringBuilder): Boolean = {

    if(isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }

    // TODO Check file spec, if doesn't exist then assume create a new file
    val p = Paths.get(fileSpec)
    val options = new util.HashSet[OpenOption]
    options.add(SPARSE)
    options.add(CREATE)
    options.add(WRITE)

    fileChannel = FileChannel.open(p, options)


    // Allocate the bytebuffer
    byteBuffer = ByteBuffer.allocate(DSK_SECTSIZE)


    attachedPath = Some(p)
    capacity = DSK_SECTSIZE * DSK_SECT * MAX_TRACKS
    dirty = false

    sb.append(s"$getName: Attached: ${attachedPath.get.getFileName}\n")
    sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity,true)}")
    // Attaching enabled the device implicitly
    setEnable(true)

    false
  }

  override def detach(sb: StringBuilder): Boolean = {

    if(!isAvailable) {
      sb.append(s"$getName: Unit is not attached.")
      return true
    }
    if(dirty) writebuf()
    fileChannel.close()
    capacity = 0
    byteBuffer.clear()

    false
  }


  def writebuf(): Unit = {
    var i = current_byte
    while (i < DSK_SECTSIZE) { // null-fill rest of sector if any
      byteBuffer.put(i, 0)
      i += 1
    }
    if (!isWriteProtect) {
      fileChannel.position(DSK_SECTSIZE * sectors_per_track * current_track +
        DSK_SECTSIZE * current_sector)
      while (byteBuffer.hasRemaining) fileChannel.write(byteBuffer)
    }

    current_byte = 0xff
    dirty = false

  }

}


object DiskCapacityFormat extends Enumeration {
  val Byte, Word = Value
}

class DiskUnitAttachedException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {
}

class UnsupportedDiskFormatException(unit: DiskUnit, message: String, cause: Throwable = null) extends UnitException {

}
