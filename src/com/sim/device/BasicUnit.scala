package com.sim.device

import java.nio.file.Path

import com.sim.memory.AddressPointer
import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/18/16.
  */
trait BasicUnit {

  def action()
  def memorySize(v :UInt)

  def examine(address: AddressPointer): UInt

  def deposit(address: AddressPointer, value: UInt)

  var filename: String
  var path: Path = _
  var hwmark: UInt = UInt(0)
  var timeout: UInt = UInt(0)
  var flags: UInt = UInt(0)
  var dynamicFlags: UInt = UInt(0)
  var capac : UInt = UInt(0)


  var attachable : Boolean = true
  var readOnly : Boolean = false
  var fixedCapacity: Boolean = false
  var sequential : Boolean = false
  var attached : Boolean = false
  var bufferable: Boolean = true
  var mustBuffer: Boolean = false
  var buffered: Boolean = false
  var readOnlyOK: Boolean = true
  var disabelable:Boolean = true
  var disabled: Boolean = true

/* These flags are only set dynamically */

  val UNIT_ATTMULT = UInt(1)
  /* Allow multiple attach commands */
  val UNIT_TM_POLL = UInt(2)
  /* TMXR Polling unit */
  val UNIT_NO_FIO = UInt(4)
  /* fileref is NOT a FILE * */
  val UNIT_DISK_CHK = UInt(10)
  /* disk data debug checking (sim_disk) */
  val UNIT_V_DF_TAPE = UInt(4)
  /* Bit offset for Tape Density reservation */
  val UNIT_S_DF_TAPE = UInt(3) /* Bits Reserved for Tape Density */


}
