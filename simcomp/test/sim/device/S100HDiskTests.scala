package sim.device

import org.junit.Assert.assertTrue
import org.junit.{Before, Test}
import sim.{TestMachine, Utils}
import sim.cpu.{BasicMMU, Register16, Z80, Z80Tests}
import sim.s100.{S100FD400Device, S100FD400Unit, S100HDSKDevice, S100HDSKUnit, S100Machine}
import sim.unsigned.{UByte, UInt}

class S100HDiskTests {

  var z80: Z80 = _
  var mmu: BasicMMU = _
  var machine: S100Machine = _
  var PC: Register16 = _
  var hdsk : S100HDSKDevice = _
  var fdsk : S100FD400Device = _
  var sb:StringBuilder = _

  @Before
  def setUpZ80(): Unit = {
    if (machine == null) {
      machine = new S100Machine()
      machine.init()
      z80 = machine.findDevice("Z80A").get.asInstanceOf[Z80]
      mmu = z80.MMU
      z80.setMemorySize(UInt(0x500))
      //Z80Tests.mmu.mapRAM(UInt(0x0000), UInt(0x500))
      PC = z80.registers("PC").asInstanceOf[Register16]
      sb = new StringBuilder
      z80.setOption("STOPONHALT", "true", sb)
      hdsk = machine.findDevice("HDA").get.asInstanceOf[S100HDSKDevice]
      fdsk = machine.findDevice("FDA").get.asInstanceOf[S100FD400Device]
      Utils.outln(sb.toString())
    sb.clear()
    }
  }

  @Test
  def testRAM(): Unit = {

    val image = hdsk.bootrom_hdsk
    mmu.mapRAM(UInt(0x5c00), UInt(image.size - 1), image.toArray)

    assertTrue(mmu.get8(0x5c00) == 0xf3)
    assertTrue(mmu.get8(0x5c70) == 0xc3)
    assertTrue(mmu.get8(0x5cff) == 0x00)

    val sb = new StringBuilder
    z80.DAsm(0x5c00, 0x5cff, sb)
    Utils.out(sb.toString())
  }

  @Test
  def testAttach():Unit = {
    val units = hdsk.getUnits.asInstanceOf[Iterator[S100HDSKUnit]]
    val unit = units.next()
    Utils.outln(s"Unit: ${unit.getName}")

    // No easy way to check return from this right now
    unit.attach("Z3DOS.DSK", sb)
    Utils.outln(sb.toString())

    sb.clear()
    unit.detach(sb)
    Utils.outln(sb.toString())
  }

  @Test
  def testReadConsist():Unit = {

    val units = hdsk.getUnits.asInstanceOf[Iterator[S100HDSKUnit]]
    val unit = units.next()
    Utils.outln(s"Unit: ${unit.getName}")

    // No easy way to check return from this right now
    unit.attach("cpm3.dsk", sb)
    Utils.outln(sb.toString())

    val funits = fdsk.getUnits.asInstanceOf[Iterator[S100FD400Unit]]
    val funit = funits.next()
    Utils.outln(s"FUnit: ${funit.getName}")
    sb.clear()
    funit.attach("cpm3.dsk", sb)
    Utils.outln(sb.toString())

    // Attached to both devices, something we couldn't physically do of course

    Utils.outln("cpm image attach to both controllers, testing we get the same bytes for both")

    fdsk.current_disk = Some(funit)
    funit.current_sector = 4
    funit.current_track = 4
    funit.seek()
    funit.readSector()  // should have read track 4, sector 4
    val fposition = funit.fileChannel.position()
    Utils.outln(s"FUNIT POSITION AFTER READ = $fposition")




    sb.clear()
    funit.detach(sb)
    Utils.outln(sb.toString())

    sb.clear()
    unit.detach(sb)
    Utils.outln(sb.toString())

  }
}
