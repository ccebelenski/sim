package com.sim.cpu

import com.sim.Utils
import com.sim.s100.S100Machine
import com.sim.unsigned.{UByte, UInt}
import org.junit.{Before, BeforeClass, Test}
import org.junit.Assert._

class Z80Tests {

  var z80: Z80 = null
  var mmu: BasicMMU = null
  var machine: S100Machine = null
  var PC: Register16 = null

  @Before
  def setUpZ80(): Unit = {
    if (Z80Tests.machine == null) {
      Z80Tests.machine = new S100Machine()
      Z80Tests.machine.init()
      Z80Tests.z80 = Z80Tests.machine.findDevice("Z80A").get.asInstanceOf[Z80]
      Z80Tests.mmu = Z80Tests.z80.MMU
      Z80Tests.z80.setMemorySize(UInt(0xFFFF))
      Z80Tests.mmu.mapRAM(UInt(0x0000), UInt(0xc000))
      Z80Tests.PC = Z80Tests.z80.registers("PC").asInstanceOf[Register16]

    }
    Z80Tests.z80.resetCPU()
    z80 = Z80Tests.z80
    mmu = Z80Tests.mmu
    machine = Z80Tests.machine
    PC = Z80Tests.PC
  }

  @Test
  def endiannessTest(): Unit = {
    z80.H.set8(UByte(0x5A.byteValue()))
    z80.L.set8(UByte(0xe5.byteValue()))
    assertTrue(z80.HL.get16 == 0x5AE5)
    z80.BC.set16(0x1234)

  }

  @Test
  // NOP
  def test0x00(): Unit = {
    mmu.put8(PC, UByte(0x00))
    mmu.put8(0x0001, UByte(0x76))
    z80.AF.set16(0x0000)
    z80.runcpu()
    assertTrue(PC.get16 == 0x01)
    assertTrue(z80.tStates == 8)
    assertTrue(z80.AF.get16 == 0x0000)

  }

  @Test
  // LD BC, nnnn
  def test0x01(): Unit = {
    // This has some other checks of the infrastructure that don't need to be repeated,
    // such as general checks for endiness, etc.
    z80.deposit(0x0000, 0x01)
    z80.depositWord(0x0001, 0x1234)
    assertTrue(z80.examine(0x0001) == 0x34)
    z80.deposit(0x0003, 0x76)
    z80.runcpu()
    assertTrue(PC.get16 == 0x0003)
    z80.examineWord(0x0001)
    assertTrue(z80.BC.get16 == 0x1234)
    assertTrue(z80.B.get8 == 0x12)
  }

  @Test
  // LD (BC), A
  def test0x02(): Unit = {
    z80.deposit(0x0000, 0x02)
    z80.deposit(0x0001, 0x76)
    z80.BC(0x0002)
    z80.A(0x20)
    z80.runcpu()
    assertTrue(z80.examine(0x0002) == 0x20)
  }

  @Test
  // INC B
  def test0x04(): Unit = {
    z80.deposit(0x0000, 0x04)
    z80.deposit(0x0001, 0x76)
    z80.B(0x20)
    z80.AF(0x0000)
    z80.runcpu()
    Utils.outln(s"${z80.B} ${z80.AF}")
    assertTrue(z80.B.get8 == 0x21)
    assertTrue(z80.F.get8 == 0x20)
    z80.B(0x02)
    z80.AF(0x0000)
    z80.PC(0x0000)
    z80.runcpu()
    Utils.outln(s"${z80.B} ${z80.AF}")
    assertTrue(z80.B.get8 == 0x03)
    assertTrue(z80.F.get8 == 0x00)

  }

  @Test
  // DEC B
  def test0x05(): Unit = {
    z80.deposit(0x0000, 0x05)
    z80.deposit(0x0001, 0x76)
    z80.B(0x21)
    z80.AF(0x0000)
    z80.runcpu()
    Utils.outln(s"${z80.B} ${z80.AF}")
    assertTrue(z80.B.get8 == 0x20)
    assertTrue(z80.F.get8 == 0x22)
    z80.B(0x20)
    z80.PC(0x0000)
    z80.runcpu()
    assertTrue(z80.B.get8 == 0x1F)
    assertTrue(z80.F.get8 == 0x1A)

  }

  @Test
  // LD B,nn
  def test0x06(): Unit = {
    z80.deposit(0x0000, 0x06)
    z80.deposit(0x0001, 0x20)
    z80.deposit(0x0002, 0x76)
    z80.runcpu()
    assertTrue(z80.B.get8 == 0x020)
    assertTrue(z80.F.get8 == 0x0000)
  }

  @Test
  // RLCA
  def test0x07(): Unit = {
    z80.deposit(0x0000, 0x07)
    z80.deposit(0x0001, 0x76)
    z80.A(0x0001)
    z80.F(16 + 128 + 2)
    z80.runcpu()
    assertTrue(z80.A.get8 == 0x0002)
    assertTrue(z80.F.get8 == 128)
  }

  @Test
  // EX AF, AF'
  def test0x08(): Unit = {
    z80.deposit(0x0000, 0x08)
    z80.deposit(0x0001, 0x76)
    z80.A(0x1A)
    z80.F(0x33)
    z80.FP(0xFF)
    z80.runcpu()
    assertTrue(z80.AP.get8 == 0x1A)
    assertTrue(z80.FP.get8 == 0x33)
    assertTrue(z80.A.get8 == 0x00)
    assertTrue(z80.F.get8 == 0xff)
  }

  @Test
  // ADD HL, BC
  def test0x09(): Unit = {
    z80.deposit(0x0000, 0x09)
    z80.deposit(0x0001, 0x76)
    z80.HL(0x0002)
    z80.BC(0x0001)
    z80.F(128)
    z80.runcpu()
    assertTrue(z80.HL.get16 == 0x0003)
    assertTrue((z80.F.get8 & 128) != 0) // Check S flag preserved
    assertTrue((z80.F.get8 & 2) == 0) // Check N cleared (add)
    assertTrue((z80.F.get8 & 1) == 0) // Carry flag clear
    z80.PC(0x0000)
    z80.HL(0xFFFF)
    z80.BC(0x0001)
    z80.runcpu()
    assertTrue(z80.HL.get16 == 0x0000)
    assertTrue((z80.F.get8 & 128) != 0) // Check S flag preserved
    assertTrue((z80.F.get8 & 2) == 0) // Check N cleared (add)
    assertTrue((z80.F.get8 & 1) != 0) // Carry flag set

  }

  @Test
  // LD A,BC
  def test0x0a(): Unit = {
    z80.deposit(0x0000, 0x0a)
    z80.deposit(0x0001, 0x76)
    z80.BC(0x0400)
    z80.deposit(0x0400, 0x7f)
    z80.runcpu()
    assertTrue(z80.A.get8 == 0x7f)
    assertTrue(z80.BC.get16 == 0x400)
    assertTrue(z80.examine(0x0400) == 0x7f)
  }

  @Test
  // DEC BC
  def test0x0b(): Unit = {
    z80.deposit(0x0000, 0x0b)
    z80.deposit(0x0001, 0x76)
    z80.BC(0x0400)
    z80.runcpu()
    assertTrue(z80.BC.get16 == 0x03FF)

  }

  @Test
  // DJNZ nn
  def test0x10(): Unit = {
    z80.deposit(0x0000, 0x10) // DJNZ
    z80.deposit(0x0001, 0x01) // relative offset, 0 based
    z80.deposit(0x0002, 0x76) // HALT
    z80.deposit(0x0003, 0x0c) // INC C
    z80.deposit(0x0004, 0x76) // HALT
    z80.B(0x04)
    z80.runcpu()
    assertTrue(z80.B.get8 == 0x03)
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.C.get8 == 0x00)
    z80.PC(0x0000)
    z80.B(0x01)
    z80.runcpu()
    assertTrue(z80.B.get8 == 0x00)
    assertTrue(z80.PC.get16 == 0x0004)
    assertTrue(z80.C.get8 == 0x01)
  }

  @Test
  def test0xd6() : Unit = {
    z80.deposit(0x0000, 0xd6)
    z80.deposit(0x0001, 0x02)
    z80.deposit(0x0002, 0x76)
    z80.PC(0x0000)
    z80.A(0x05)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0x03)
    assertTrue(!z80.testFlag(z80.F,z80.FLAG_Z))
    assertTrue(!z80.testFlag(z80.F,z80.FLAG_P))
    assertTrue(z80.testFlag(z80.F,z80.FLAG_N))


  }
  @Test
  // HALT
  def test0x76(): Unit = {
    mmu.put8(Z80Tests.PC, UByte(0x76))
    z80.runcpu()
    assertTrue(z80.tStates == 4)
    assertTrue(PC.get16 == 0)
  }


  @Test
  def test0xdd0x34() : Unit = {
    // INC (IX+dd)
    z80.deposit(0x0000, 0xdd)
    z80.deposit(0x0001, 0x34)
    z80.deposit(0x0002, 0x01)
    z80.deposit(0x0003, 0x76)
    z80.PC(0x0000)
    z80.IX(0x4000)
    z80.deposit(0x4001, 0x01)
    z80.runcpu()
    assertTrue(z80.IX.get16 == 0x4000)
    assertTrue(z80.examine(0x4001).intValue == 0x02)
  }
}

object Z80Tests {
  var z80: Z80 = null
  var mmu: BasicMMU = null
  var machine: S100Machine = null
  var PC: Register16 = null
  val HALT = 0x76
}