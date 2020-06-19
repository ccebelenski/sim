package sim.s100

import java.nio.ByteBuffer

import sim.Utils
import sim.cpu.Z80MMU
import sim.device.{BinaryUnitOption, Bootable, DiskInfo, PortMappedDiskDevice, SupportsOptions}
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

  // Buffer for read/write - null initially
  var hdiskBuf: ByteBuffer = _
  var hostSector: Int = 0


  override def optionChanged(sb: StringBuilder): Unit = ???

  override def createUnitOptions: Unit = {

    unitOptions.append(BinaryUnitOption("ALTAIRROM", "Use modified Altair boot ROM", value = false))
    S100HDSKDevice.DPB.foreach(pb => unitOptions.append(BinaryUnitOption(pb.name, pb.desc, value = false)))
  }


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
  var selectedDMA: Int = 0

  private def hdsk_checkParameters(): Boolean = {
    if (current_disk.isEmpty) current_disk = findUnitByNumber(0).asInstanceOf[Option[S100HDSKUnit]]
    val cd = current_disk.get
    if (!cd.isAvailable) {
      Utils.outln(s"$getName: Unit is not available.")
      return false
    }
    if (cd.current_sector < 0 || cd.current_sector >= cd.HDSK_SECTORS_PER_TRACK) {
      Utils.outln(s"$getName: Constraint violation 0 < Sector=${cd.current_sector} >= ${cd.HDSK_SECTORS_PER_TRACK}, will use sector 0 instead.")
      cd.current_sector = 0
    }
    if (cd.current_track < 0 || cd.current_track >= cd.HDSK_NUMBER_OF_TRACKS) {
      Utils.outln(s"$getName: Constraint violation 0 < Track=${cd.current_track} >= ${cd.HDSK_NUMBER_OF_TRACKS}, will use sector 0 instead.")
      cd.current_track = 0

    }

    if(hdskLastCommand == HDSK_READ) {
      Utils.outlnd(current_disk.get, s"Read Track=$selectedTrack Sector=$selectedSector Len=${current_disk.get.HDSK_SECTOR_SIZE} DMA=${selectedDMA}")
    } else if(hdskLastCommand == HDSK_WRITE) {
      Utils.outlnd(current_disk.get, s"Write Track=$selectedTrack Sector=$selectedSector Len=${current_disk.get.HDSK_SECTOR_SIZE} DMA=${selectedDMA}")
    }
    true
  }

  private def doSeek(): Unit = {
    val cd = current_disk.get
    val geom = cd.HDSK_FORMAT_TYPE.get
    hostSector = if (geom.skew.isEmpty) cd.current_sector else geom.skew.get(cd.current_sector)
    val sectorSize = if (cd.HDSK_FORMAT_TYPE.get.physicalSectorSize == 0) cd.HDSK_SECTOR_SIZE else cd.HDSK_FORMAT_TYPE.get.physicalSectorSize
    cd.fileChannel.position(sectorSize * cd.HDSK_SECTORS_PER_TRACK * selectedTrack + hostSector + cd.HDSK_FORMAT_TYPE.get.offset)

  }

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {
    if (!isWrite) hdsk_in(action) else hdsk_out(action, value)
  }

  val CPM_OK: UByte = UByte(0)
  /* indicates to CP/M everything ok          */
  val CPM_ERROR: UByte = UByte(1)
  /* indicates to CP/M an error condition     */
  val CPM_EMPTY: UByte = UByte(0xe5.byteValue())
  /* default value for non-existing bytes     */
  val HDSK_NONE = 0
  val HDSK_RESET = 1
  val HDSK_READ = 2
  val HDSK_WRITE = 3
  val HDSK_PARAM = 4
  var hdskLastCommand: Int = HDSK_NONE
  var hdskCommandPosition: Int = 0
  var parameterCount: Int = 0
  var selectedDisk: Int = 0
  var selectedSector: Int = 0
  var selectedTrack: Int = 0

  val PARAMETER_BLOCK_SIZE = 19
  val parameterBlock: Array[UByte] = Array.ofDim(PARAMETER_BLOCK_SIZE)

  def hdsk_in(port: UInt): UByte = {
    if ((hdskCommandPosition == 6) && ((hdskLastCommand == HDSK_READ) || (hdskLastCommand == HDSK_WRITE))) {
      val result = if (hdsk_checkParameters()) {
        if (hdskLastCommand == HDSK_READ) hdsk_read() else hdsk_write()
      }
      else CPM_ERROR
      hdskLastCommand = HDSK_NONE
      hdskCommandPosition = 0
      return result
    }
    if (hdskLastCommand == HDSK_PARAM) {
      parameterCount += 1
      if (parameterCount >= PARAMETER_BLOCK_SIZE)
        hdskLastCommand = HDSK_NONE
      return parameterBlock(parameterCount - 1)
    }
    Utils.outln(s"$getName Illegal IN command detected (port=$port, cmd=$hdskLastCommand, pos=$hdskCommandPosition).")
    CPM_OK
  }


  def hdsk_out(port: UInt, data: UByte): UByte = {

    var unit: Option[S100HDSKUnit] = None
    var current: S100HDiskParamsBase = null

    hdskLastCommand match {

      case HDSK_PARAM =>
        parameterCount = 0
        val thisDisk = if ((0 <= data) && (data < S100HDSKDevice.HDSK_NUMBER)) data.intValue else 0
        unit = findUnitByNumber(thisDisk).asInstanceOf[Option[S100HDSKUnit]]
        if (unit.get.isAvailable) {
          current = unit.get.HDSK_FORMAT_TYPE.get
          parameterBlock(17) = UByte((unit.get.HDSK_SECTOR_SIZE & 0xff).byteValue())
          parameterBlock(18) = UByte(((unit.get.HDSK_SECTOR_SIZE >> 8) & 0xff).byteValue())
        }
        else {
          current = S100HDSKDevice.DPB.head
          parameterBlock(17) = UByte(128.byteValue())
          parameterBlock(18) = UByte(0)
        }
        parameterBlock(0) = UByte((current.spt & 0xff).byteValue())
        parameterBlock(1) = UByte(((current.spt >> 8) & 0xff).byteValue())
        parameterBlock(2) = UByte(current.bsh.byteValue())
        parameterBlock(3) = UByte(current.blm.byteValue())
        parameterBlock(4) = UByte(current.exm.byteValue())
        parameterBlock(5) = UByte((current.dsm & 0xff).byteValue())
        parameterBlock(6) = UByte(((current.dsm >> 8) & 0xff).byteValue())
        parameterBlock(7) = UByte((current.drm & 0xff).byteValue())
        parameterBlock(8) = UByte(((current.drm >> 8) & 0xff).byteValue())
        parameterBlock(9) = UByte(current.al0.byteValue())
        parameterBlock(10) = UByte(current.al1.byteValue())
        parameterBlock(11) = UByte((current.cks & 0xff).byteValue())
        parameterBlock(12) = UByte(((current.cks >> 8) & 0xff).byteValue())
        parameterBlock(13) = UByte((current.off & 0xff).byteValue())
        parameterBlock(14) = UByte(((current.off >> 8) & 0xff).byteValue())
        parameterBlock(15) = UByte(current.psh.byteValue())
        parameterBlock(16) = UByte(current.phm.byteValue())

      case HDSK_WRITE | HDSK_READ =>
        hdskCommandPosition match {

          case 0 =>
            selectedDisk = data
            hdskCommandPosition += 1

          case 1 =>
            selectedSector = data
            hdskCommandPosition += 1

          case 2 =>
            selectedTrack = data
            hdskCommandPosition += 1

          case 3 =>
            selectedTrack += (data << 8)
            hdskCommandPosition += 1

          case 4 =>
            selectedDMA = data
            hdskCommandPosition += 1

          case 5 =>
            selectedDMA += (data << 8)
            hdskCommandPosition += 1

          case _ =>
            hdskLastCommand = HDSK_NONE
            hdskCommandPosition = 0
        }

      case _ =>
        if ((HDSK_RESET <= data) && (data <= HDSK_PARAM))
          hdskLastCommand = data
        else {
          Utils.outln(s"$getName Illegal OUT command detected (port=$port, cmd=$data).")
          hdskLastCommand = HDSK_RESET
        }
        hdskCommandPosition = 0
    }
    UByte(0) /* ignored, since OUT */
  }

  /* pre-condition: hdsk_checkParameters has been executed to repair any faulty parameters */
  def hdsk_read(): UByte = {
    val unit = findUnitByNumber(selectedDisk).asInstanceOf[S100HDSKUnit]

    if (unit.isIMD()) {
      val thisDisk: DiskInfo = unit.diskInfo
      hostSector = unit.HDSK_FORMAT_TYPE match {
        case None => selectedSector
        case Some(x) => x.skew match {
          case None => selectedSector
          case Some(y) => y(selectedSector)
        }
      }
      +thisDisk.track(1)(0).start_sector

      val sectorSize = if (unit.HDSK_FORMAT_TYPE.get.physicalSectorSize == 0) unit.HDSK_SECTOR_SIZE else
        unit.HDSK_FORMAT_TYPE.get.physicalSectorSize

      var cylinder: Int = selectedTrack
      var head: Int = 0
      if (cylinder >= thisDisk.ntracks / thisDisk.nsides) {
        head = 1
        cylinder -= thisDisk.ntracks / thisDisk.nsides
      }

      hdiskBuf = ByteBuffer.allocate(sectorSize)

      // (flags, readln)
      val result = unit.sectRead(unit, cylinder, head, hostSector, hdiskBuf)
      if (result._2 == 0) {
        for (i <- 0 to unit.HDSK_SECTOR_SIZE) hdiskBuf.put(CPM_EMPTY.byteValue)
        Utils.outln(s"$getName Could not read Sector=$selectedSector Track=$selectedTrack.")
       return  CPM_ERROR
      }
    } else {
      doSeek()
      hdiskBuf = ByteBuffer.allocate(unit.HDSK_SECTOR_SIZE)
      val read = unit.fileChannel.read(hdiskBuf)
      if (read <= 0) {
        hdiskBuf.clear()
        for (i <- 0 to unit.HDSK_SECTOR_SIZE) hdiskBuf.put(CPM_EMPTY.byteValue)
        Utils.outln(s"$getName Could not read Sector=$selectedSector Track=$selectedTrack.")
        return CPM_OK /* allows the creation of empty hard disks */
      }
    }
    for (i <- 0 to unit.HDSK_SECTOR_SIZE) machine.cpu.MMU.put8(selectedDMA + i, UByte(hdiskBuf.get(i)))

    CPM_OK
  }

  /* pre-condition: hdsk_checkParameters has been executed to repair any faulty parameters */
  def hdsk_write(): UByte = {
    val unit = findUnitByNumber(selectedDisk).get.asInstanceOf[S100HDSKUnit]

    // TODO Check for write enabled
    if (unit.isEnabled) {
      /* write enabled */
      hdiskBuf = ByteBuffer.allocate(unit.HDSK_SECTOR_SIZE)
      for (i <- 0 to unit.HDSK_SECTOR_SIZE) hdiskBuf.put(machine.cpu.MMU.get8(selectedDMA + i).byteValue)
      if (unit.isIMD()) {
        val thisDisk: DiskInfo = unit.diskInfo
        hostSector = unit.HDSK_FORMAT_TYPE match {
          case None => selectedSector
          case Some(x) => x.skew match {
            case None => selectedSector
            case Some(y) => y(selectedSector)
          }
        }
        +thisDisk.track(1)(0).start_sector
        val sectorSize = if (unit.HDSK_FORMAT_TYPE.get.physicalSectorSize == 0) unit.HDSK_SECTOR_SIZE else
          unit.HDSK_FORMAT_TYPE.get.physicalSectorSize


        var cylinder = selectedTrack
        var head = 0
        if (cylinder >= thisDisk.ntracks / thisDisk.nsides) {
          head = 1
          cylinder = cylinder - thisDisk.ntracks / thisDisk.nsides
        }
        val result = unit.sectWrite(unit, 0, cylinder, head, hostSector, hdiskBuf)
        if (result._1 != 0) {
          Utils.outln(s"$getName  Could not write Sector=$selectedSector Track=$selectedTrack.")
          return CPM_ERROR
        }
      } else {
        doSeek()
        val rtn = unit.fileChannel.write(hdiskBuf)

        if (rtn != unit.HDSK_SECTOR_SIZE) {
          Utils.outln(s"$getName Could not write Sector=$selectedSector Track=$selectedTrack Result=$rtn.")
          return CPM_ERROR
        }
      }
    }
    else {
      Utils.outln(s"$getName Could not write to locked disk Sector=$selectedSector Track=$selectedTrack.")
      return CPM_ERROR
    }
    CPM_OK
  }

  /* flush all attached drives. Returns CPM_OK if everything fine, otherwise CPM_ERROR */
  // Not needed for our I/O methods?
  def hdsk_flush(): Int = {
    CPM_OK
  }


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

    //val cd = unit.get
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


  private def assignFormat(unit: S100HDSKUnit): Unit = {
    unit.HDSK_FORMAT_TYPE = S100HDSKDevice.DPB.find(_.capac == unit.capacity)
  }

}

object S100HDSKDevice {
  val HDSK_CAPACITY: Long = 2048 * 32 * 128 //Default Altair HDSK Capacity
  val HDSK_NUMBER: Int = 8 //number of HDSK
  val SPT16: Int = 16
  val SPT32: Int = 32
  val SPT26: Int = 26
  val SPT52: Int = 52

  val DPB = List(new HDSK, new CPM68K, new EZ80FL, new P112, new SU720,
    new OSB1, new OSB2, new NSSS1, new NSSS2, new NSDS2, new VGSS,
    new VGDS, new DISK1A, new SSSD8, new SSSD8S, new SSDD8,
    new SSDD8S, new DSDD8, new DSDD8S, new D512SSDD8, new D512DSDD8,
    new APPLEDO, new APPLEPO, new APPLED2, new APPLEP2, new MITS,
    new MITS2, new V1050)
}
