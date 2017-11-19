package com.sim.device

import com.sim.unsigned.UInt

import scala.collection.mutable.ArrayBuffer

/**
  * Created by christophercebelenski on 7/1/16.
  */
trait BasicDevice {

  var name: String
  var description: String
  var units: ArrayBuffer[BasicUnit] = new ArrayBuffer[BasicUnit]
  var flags: UInt
  var logicalName: String
  // Address radix
  var aradix: UInt
  // address width
  var awidth: UInt
  // Address increment
  var aincr: UInt
  // data radix
  var dradix: UInt
  // data width
  var dwidth: UInt



  def reset()


  def boot(u: UInt)

  def attach(unit: BasicUnit)

  def detach(unit: BasicUnit)


  def help()



}

object BasicDevice {
  /* Device flags */

  val DEV_V_DIS = UInt(0)
  /* dev disabled */
  val DEV_V_DISABLE = UInt(1)
  /* dev disable-able */
  val DEV_V_DYNM = UInt(2)
  /* mem size dynamic */
  val DEV_V_DEBUG = UInt(3)
  /* debug capability */
  val DEV_V_TYPE = UInt(4)
  /* Attach type */
  val DEV_S_TYPE = UInt(3)
  /* Width of Type Field */
  val DEV_V_SECTORS = UInt(7)
  /* Unit Capacity is in 512byte sectors */
  val DEV_V_DONTAUTO = UInt(8)
  /* Do not auto detach already attached units */
  val DEV_V_FLATHELP = UInt(9)
  /* Use traditional (unstructured) help */
  val DEV_V_NOSAVE = UInt(10)
  /* Don't save device state */
  val DEV_V_UF = UInt(16)
  /* user flags */
  val DEV_V_RSV = UInt(31)                           /* reserved */

  val DEV_DIS: UInt = UInt(1) << DEV_V_DIS
  /* device is currently disabled */
  val DEV_DISABLE: UInt = UInt(1) << DEV_V_DISABLE
  /* device can be set enabled or disabled */
  val DEV_DYNM: UInt = UInt(1) << DEV_V_DYNM
  /* device requires call on msize routine to change memory size */
  val DEV_DEBUG: UInt = UInt(1) << DEV_V_DEBUG
  /* device supports SET DEBUG command */
  val DEV_SECTORS: UInt = UInt(1) << DEV_V_SECTORS
  /* capacity is 512 byte sectors */
  val DEV_DONTAUTO: UInt = UInt(1) << DEV_V_DONTAUTO
  /* Do not auto detach already attached units */
  val DEV_FLATHELP: UInt = UInt(1) << DEV_V_FLATHELP
  /* Use traditional (unstructured) help */
  val DEV_NOSAVE: UInt = UInt(1) << DEV_V_NOSAVE
  /* Don't save device state */


  val DEV_TYPEMASK: UInt = ((UInt(1) << DEV_S_TYPE) - UInt(1)) << DEV_V_TYPE
  val DEV_DISK: UInt = UInt(1) << DEV_V_TYPE
  /* sim_disk Attach */
  val DEV_TAPE: UInt = UInt(2) << DEV_V_TYPE
  /* sim_tape Attach */
  val DEV_MUX: UInt = UInt(3) << DEV_V_TYPE
  /* sim_tmxr Attach */
  val DEV_ETHER: UInt = UInt(4) << DEV_V_TYPE
  /* Ethernet Device */
  val DEV_DISPLAY: UInt = UInt(5) << DEV_V_TYPE

  val DEV_UFMASK: Int = ((UInt(1) << DEV_V_RSV) - 1) & ~((UInt(1) << DEV_V_UF) - UInt(1))
  val DEV_RFLAGS: Int = DEV_UFMASK | DEV_DIS/* restored flags */
}