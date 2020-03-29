package sim.s100

import sim.Utils
import sim.cpu.Z80MMU
import sim.device.{Bootable, PortMappedDiskDevice, SupportsOptions}
import sim.unsigned.{UByte, UInt}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class S100HDSKDevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDiskDevice(machine, mmu, ports)
  with SupportsOptions with Bootable {

  override val description: String = "Hard Disk"
  override val name = "HD"
  override val supportsBoot: Boolean = true

  override def init(): Unit = {
    // Create 16 units
    for (i <- 0 until S100HDSKDevice.HDSK_NUMBER) {
      val du = new S100HDSKUnit(this)
      addUnit(du)
    }

  }

  override def optionChanged(sb: StringBuilder): Unit = ???



  /*  The hard disk port is 0xfd. It understands the following commands.
      1.  Reset
          ld  b,32
          ld  a,HDSK_RESET
      l:  out (0fdh),a
          dec b
          jp  nz,l
      2.  Read / write
          ; parameter block
          cmd:        db  HDSK_READ or HDSK_WRITE
          hd:         db  0   ; 0 .. 7, defines hard disk to be used
          sector:     db  0   ; 0 .. 31, defines sector
          track:      dw  0   ; 0 .. 2047, defines track
          dma:        dw  0   ; defines where result is placed in memory
          ; routine to execute
          ld  b,7             ; size of parameter block
          ld  hl,cmd          ; start address of parameter block
      l:  ld  a,(hl)          ; get byte of parameter block
          out (0fdh),a        ; send it to port
          inc hl              ; point to next byte
          dec b               ; decrement counter
          jp  nz,l            ; again, if not done
          in  a,(0fdh)        ; get result code
      3.  Retrieve Disk Parameters from controller (Howard M. Harte)
          Reads a 19-byte parameter block from the disk controller.
          This parameter block is in CP/M DPB format for the first 17 bytes,
          and the last two bytes are the lsb/msb of the disk's physical
          sector size.
          ; routine to execute
          ld   a,hdskParam    ; hdskParam = 4
          out  (hdskPort),a   ; Send 'get parameters' command, hdskPort = 0fdh
          ld   a,(diskno)
          out  (hdskPort),a   ; Send selected HDSK number
          ld   b,17
      1:  in   a,(hdskPort)   ; Read 17-bytes of DPB
          ld   (hl), a
          inc  hl
          djnz 1
          in   a,(hdskPort)   ; Read LSB of disk's physical sector size.
          ld   (hsecsiz), a
          in   a,(hdskPort)   ; Read MSB of disk's physical sector size.
          ld   (hsecsiz+1), a
  */
  var current_disk: Option[S100HDSKUnit] = None
  var selectedDMA : Int = 0

  private def hdsk_checkParameters() : Boolean = {
    if(current_disk.isEmpty) current_disk = findUnitByNumber(0).asInstanceOf[Option[S100HDSKUnit]]
    val cd = current_disk.get
    if(!cd.isAvailable) {
      Utils.outln(s"$getName: Unit is not available.")
      return false
    }
    if(cd.current_sector < 0 || cd.current_sector >= cd.DSK_SECT) {
      Utils.outln(s"$getName: Constraint violation 0 <= Sector=${cd.current_sector} < ${cd.DSK_SECT}, will use sector 0 instead.")
      cd.current_sector = 0
    }
    if(cd.current_track <0 || cd.current_track >= cd.MAX_TRACKS) {
      Utils.outln(s"$getName: Constraint violation 0 <= Track=${cd.current_track} < ${cd.MAX_TRACKS}, will use sector 0 instead.")
      cd.current_track= 0

    }

    true
  }

  private def doSeek():Unit = {
    val cd = current_disk.get
    val geom = cd.HDSK_FORMAT_TYPE.get
    val hostSector = if(geom.skew.isEmpty) cd.current_sector else geom.skew.get(cd.current_sector)

  }
  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???

  val HDSK_BOOT_ADDRESS: Int = 0x5c00
  val bootrom_hdsk: mutable.ListBuffer[Int] = ListBuffer(
    0xf3, 0x06, 0x80, 0x3e, 0x0e, 0xd3, 0xfe, 0x05, /* 5c00-5c07 */
    0xc2, 0x05, 0x5c, 0x3e, 0x16, 0xd3, 0xfe, 0x3e, /* 5c08-5c0f */
    0x12, 0xd3, 0xfe, 0xdb, 0xfe, 0xb7, 0xca, 0x20, /* 5c10-5c17 */
    0x5c, 0x3e, 0x0c, 0xd3, 0xfe, 0xaf, 0xd3, 0xfe, /* 5c18-5c1f */
    0x06, 0x20, 0x3e, 0x01, 0xd3, 0xfd, 0x05, 0xc2, /* 5c20-5c27 */
    0x24, 0x5c, 0x11, 0x08, 0x00, 0x21, 0x00, 0x00, /* 5c28-5c2f */
    0x0e, 0xb8, 0x3e, 0x02, 0xd3, 0xfd, 0x3a, 0x37, /* 5c30-5c37 */
    0xff, 0xd6, 0x08, 0xd3, 0xfd, 0x7b, 0xd3, 0xfd, /* 5c38-5c3f */
    0x7a, 0xd3, 0xfd, 0xaf, 0xd3, 0xfd, 0x7d, 0xd3, /* 5c40-5c47 */
    0xfd, 0x7c, 0xd3, 0xfd, 0xdb, 0xfd, 0xb7, 0xca, /* 5c48-5c4f */
    0x53, 0x5c, 0x76, 0x79, 0x0e, 0x80, 0x09, 0x4f, /* 5c50-5c57 */
    0x0d, 0xc2, 0x60, 0x5c, 0xfb, 0xc3, 0x00, 0x00, /* 5c58-5c5f */
    0x1c, 0x1c, 0x7b, 0xfe, 0x20, 0xca, 0x73, 0x5c, /* 5c60-5c67 */
    0xfe, 0x21, 0xc2, 0x32, 0x5c, 0x1e, 0x00, 0x14, /* 5c68-5c6f */
    0xc3, 0x32, 0x5c, 0x1e, 0x01, 0xc3, 0x32, 0x5c, /* 5c70-5c77 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5c78-5c7f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5c80-5c87 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5c88-5c8f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5c90-5c97 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5c98-5c9f */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5ca0-5ca7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5ca8-5caf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cb0-5cb7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cb8-5cbf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cc0-5cc7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cc8-5ccf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cd0-5cd7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cd8-5cdf */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5ce0-5ce7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5ce8-5cef */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cf0-5cf7 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 5cf8-5cff */
  )


  override def boot(unitno: Int, sb: StringBuilder): Boolean = {
    val unit = findUnitByNumber(unitno).asInstanceOf[Option[S100FD400Unit]]
    if (unit.isEmpty || !unit.get.isAvailable) {
      sb.append(s"$getName: Unit is not available.")
      return false
    } // No such unit?

    val cd = unit.get
    val useAltairROM = getBinaryOption("ALTAIRROM") | machine.getCPU.isBanked

    if (machine.cpu.getMemorySize < UInt(24 * 1024)) {
      sb.append(s"$getName: Need at least 24KB RAM to boot from hard disk.")
      return false
    }

    if (useAltairROM) {
      sb.append(s"$getName: Device not supported by ALTAIRROM option.")
      return false
    }
    mmu.installROM(bootrom_hdsk.toArray,
      bootrom_hdsk.size, UInt(HDSK_BOOT_ADDRESS))

    machine.getCPU.PC(HDSK_BOOT_ADDRESS)
    sb.append(f"$getName: Boot ROM start: $HDSK_BOOT_ADDRESS%04x")
    machine.getCPU.runcpu()
    true
  }


  private def assignFormat(unit:S100HDSKUnit): Unit = {
    unit.HDSK_FORMAT_TYPE = S100HDSKDevice.dpb.find(_.capac == unit.capacity)
  }

}

object S100HDSKDevice {
  val HDSK_CAPACITY: Long = 2048 * 32 * 128 //Default Altair HDSK Capacity
  val HDSK_NUMBER: Int = 16 //number of hard disks
  val SPT16: Int = 16
  val SPT32: Int = 32
  val SPT26: Int = 26
  val SPT52: Int = 52

  val dpb = List(new HDSK, new CPM68K, new EZ80FL, new P112, new SU720,
    new OSB1, new OSB2, new NSSS1, new NSSS2, new NSDS2, new VGSS,
    new VGDS, new DISK1A, new SSSD8, new SSSD8S, new SSDD8,
    new SSDD8S, new DSDD8, new DSDD8S, new D512SSDD8, new D512DSDD8,
    new APPLEDO, new APPLEPO, new APPLED2, new APPLEP2, new MITS,
    new MITS2, new V1050)
}
