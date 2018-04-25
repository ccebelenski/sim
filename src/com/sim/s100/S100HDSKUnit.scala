package com.sim.s100

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{OpenOption, Path, Paths}
import java.nio.file.StandardOpenOption.{CREATE, READ, SPARSE, WRITE}
import java.util

import com.sim.Utils
import com.sim.device.{BasicUnit, DiskUnit}

class S100HDSKUnit(device:S100HDSKDevice) extends BasicUnit(device) with  DiskUnit {

  // Format type - actually the parameters for the type
  var HDSK_FORMAT_TYPE : Option[S100HDiskParamsBase] = None

  val HDSK_CAPACITY :Int = 2048 * 32 * 128 // Default Altair HDSK capacity
  val HDSK_MAX_SECTOR_SIZE :Int = 1024 // maximum size of a sector




  override val waitTime: Long = 0L

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def init(): Unit = {
    // TODO
  }

  override def MAX_TRACKS: Int = ???

  override def DSK_SECT: Int = ???

  override def DSK_SECTSIZE: Int = ???

  override def optionChanged(sb: StringBuilder): Unit = ???


  override def attach(fileSpec: String, sb: StringBuilder): Boolean = {

    if (isAvailable) {
      sb.append(s"$getName: Unit is still attached.   DETACH first.\n")
      return true
    }


    //  if doesn't exist then assume create a new file
    val p :Path = Paths.get(fileSpec)

    if(isIMD(p)) {
      sb.append(s"$getName: IMD disk images not yet supported.\n")
      return true
    }

    val options = new util.HashSet[OpenOption]
    options.add(SPARSE)
    options.add(CREATE)
    options.add(WRITE)
    options.add(READ)

    // Optionally set up some drive parameters basic on the file.
    setDriveAttributes(p)

    fileChannel = FileChannel.open(p, options)

    capacity = fileChannel.size()
    if(capacity == 0) capacity = HDSK_CAPACITY

    assignFormat()


    HDSK_FORMAT_TYPE match {
      case None =>
        // No disk parameter block found
        HDSK_FORMAT_TYPE = Some(S100HDSKDevice(0))
        sb.append(s"$getName: WARNING: Unsupported disk capacity, assuming HDSK type with capacity ${Utils.formatBytes(capacity,false)}")
      case Some(ft) =>

    }



    // Allocate the bytebuffer
    byteBuffer = ByteBuffer.allocate(DSK_SECTSIZE)


    attachedPath = Some(p)
//    capacity = DSK_SECTSIZE * DSK_SECT * MAX_TRACKS
    dirty = false

    sb.append(s"$getName: Attached: ${attachedPath.get.getFileName}\n")
    sb.append(s"$getName: Capacity: ${Utils.formatBytes(capacity, false)}")
    // Attaching enabled the device implicitly
    setEnable(true)

    false
  }


  override def detach(sb: StringBuilder): Boolean = {
    val ret = super.detach(sb)
    HDSK_FORMAT_TYPE = None

    ret
  }

  private def assignFormat():Unit = {}
}
