package com.sim.cpu

import com.sim.Utils
import com.sim.s100.{S100FD400Device, S100Machine}
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
      Z80Tests.z80.setMemorySize(UInt(0x500))
      //Z80Tests.mmu.mapRAM(UInt(0x0000), UInt(0x500))
      Z80Tests.PC = Z80Tests.z80.registers("PC").asInstanceOf[Register16]
      val sb: StringBuilder = new StringBuilder
      Z80Tests.z80.setOption("STOPONHALT", "true", sb)
      Utils.outln(sb.toString())

    }
    Z80Tests.z80.resetCPU()
    z80 = Z80Tests.z80
    mmu = Z80Tests.mmu
    machine = Z80Tests.machine
    PC = Z80Tests.PC
  }

  @Test
  def testROM(): Unit = {

    val image = S100FD400Device.alt_bootrom_dsk
    mmu.mapROM(UInt(0xff00), UInt(image.size - 1), image.toArray)

    assertTrue(mmu.get8(0xff00) == 0x21)
    assertTrue(mmu.get8(0xffff) == 0x00)
    assertTrue(mmu.get8(0xff70) == 0x08)

    // Write to ROM (expect console output)
    mmu.put8(0xff70, UByte(0xff.byteValue()))
    assertTrue(mmu.get8(0xff70) == 0x08) // Verify value did not change.

    val sb = new StringBuilder
    z80.DAsm(0xff00, 0xffff, sb)
    Utils.out(sb.toString())
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
    assertTrue((z80.F.get8 & 1) == 1) // Carry flag Set

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
    z80.PC(0x0000)
    z80.B(0x01)
    z80.runcpu()
    assertTrue(z80.B.get8 == 0x00)
    assertTrue(z80.PC.get16 == 0x0004)
  }

  @Test
  // LD A,(nnnn)
  def test0x3a(): Unit = {
    z80.deposit(0x0000, 0x3a) // LD A,(nnnn)
    z80.deposit(0x0001, 0xA0) // 0x00A0
    z80.deposit(0x0002, 0x00)
    z80.deposit(0x0003, 0x76) // HALT
    z80.deposit(0x00A0, 0x01)
    z80.PC(0x0000)
    z80.A(0x00)
    z80.runcpu()
    assertTrue(z80.A.get8 == 0x01)
    assertTrue(z80.PC.get16 == 0x0003)
  }

  @Test
  def test0xd6(): Unit = {
    z80.deposit(0x0000, 0xd6)
    z80.deposit(0x0001, 0x01)
    z80.deposit(0x0002, 0x76)
    z80.PC(0x0000)
    z80.A(0x05)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0x04)
    assertTrue(!z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(!z80.testFlag(z80.F, z80.FLAG_P))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_N))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.A(0x01)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0x00)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_P))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_N))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.A(0x00)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0xFF)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z)) // Not sure about this one.
    assertFalse(z80.testFlag(z80.F, z80.FLAG_P))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_N))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))


  }

  @Test
  // JR NZ
  def test0x20(): Unit = {
    z80.deposit(0x0000, 0x20) // JR NZ
    z80.deposit(0x0001, 0x01) // 1
    z80.deposit(0x0002, 0x3C) // INC A
    z80.deposit(0x0003, 0x3D) // DEC A
    z80.deposit(0x0004, 0x3C) // INC A
    z80.deposit(0x0005, 0xC0) // RET NZ
    z80.deposit(0x0006, 0x20) // JR NZ
    z80.deposit(0x0007, 0x01) // 1
    z80.deposit(0x0008, 0x3C) // INC A
    z80.deposit(0x0009, 0x76) // HALT
    z80.PC(0x0000)
    z80.resetCPU()
    z80.runcpu()
    assertEquals(0x01, z80.A.get8.intValue)
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
  def test0xdd0x34(): Unit = {
    // INC (IX+dd)
    z80.deposit(0x0000, 0xdd)
    z80.deposit(0x0001, 0x34)
    z80.deposit(0x0002, 0x01)
    z80.deposit(0x0003, 0x76)
    z80.PC(0x0000)
    z80.IX(0x0100)
    z80.deposit(0x0101, 0x01)
    z80.runcpu()
    assertTrue(z80.IX.get16 == 0x100)
    assertTrue(z80.examine(0x101).intValue == 0x02)
  }

  @Test
  // ADD A,A
  def test0x87(): Unit = {
    z80.deposit(0x0000, 0x87) // ADD A,A
    z80.deposit(0x0001, 0x76) // HALT

    z80.PC(0x0000)
    z80.A(0x01)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0001)
    assertTrue(z80.A.get8 == 0x02)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))


    z80.PC(0x0000)
    z80.A(0x00)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0001)
    assertTrue(z80.A.get8 == 0x00)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))

    z80.PC(0x0000)
    z80.A(0xA3)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0001)
    assertTrue(z80.A.get8 == 0x46)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
  }

  @Test
  // LD A,D
  def test0x7a(): Unit = {

    // NO flag affected
    z80.deposit(0x0000, 0x7A) // LD A,D
    z80.deposit(0x0001, 0x76) // HALT

    z80.PC(0x0000)
    z80.D(0x00)
    z80.A(0xDE) // Set to something
    z80.F(0x00) // Clear all flags - they should remain clear.
    z80.runcpu()

    assertTrue(z80.PC.get16 == 0x0001)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.get8 == 0x00)

    z80.PC(0x0000)
    z80.D(0xAA)
    z80.A(0xDE) // Set to something
    z80.F(0x00) // Clear all flags - they should remain clear.
    z80.runcpu()

    assertTrue(z80.PC.get16 == 0x0001)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.get8 == 0xAA)
  }

  @Test
  // LD A,E
  def test0x7b(): Unit = {
    z80.deposit(0x0000, 0x7B) // LD A,E
    z80.deposit(0x0001, 0x76) // HALT

    z80.PC(0x0000)
    z80.E(0x00)
    z80.A(0xDE) // Set to something
    z80.F(0x00) // Clear all flags - they should remain clear.
    z80.runcpu()

    assertTrue(z80.PC.get16 == 0x0001)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.get8 == 0x00)

    z80.PC(0x0000)
    z80.E(0xAA)
    z80.A(0xDE) // Set to something
    z80.F(0x00) // Clear all flags - they should remain clear.
    z80.runcpu()

    assertTrue(z80.PC.get16 == 0x0001)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.get8 == 0xAA)
  }

  @Test
  // CP nn
  def test0xfe(): Unit = {
    z80.deposit(0x0000, 0xFE) // CP NN
    z80.deposit(0x0001, 0x0A) // 10
    z80.deposit(0x0002, 0x76) // HLT

    z80.PC(0x0000)
    z80.A(0x00)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0x00)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.A(0x20)
    z80.runcpu()
    assertTrue(z80.A.get8 == 0x20)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.A(0x0A)
    z80.runcpu()
    assertTrue(z80.A.get8 == 0x0A)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.deposit(0x0001, 0xFF)
    z80.PC(0x0000)
    z80.A(0x00)
    z80.runcpu()
    assertTrue(!z80.testFlag(z80.F, z80.FLAG_P))
  }

  @Test
  def testHexToNum(): Unit = {
    // Takes DE as ASCII HEX (uppercase only), convert to binary in A
    z80.deposit(0x0000, 0x7a) // LD A,D
    z80.deposit(0x0001, 0x00) // NOP
    z80.deposit(0x0002, 0xCD) // CALL nnnn
    z80.deposit(0x0003, 0x00)
    z80.deposit(0x0004, 0x01) // CALL 0100
    z80.deposit(0x0005, 0x87) // ADD A,A
    z80.deposit(0x0006, 0x87) // ADD A,A
    z80.deposit(0x0007, 0x87) // ADD A,A
    z80.deposit(0x0008, 0x87) // ADD A,A
    z80.deposit(0x0009, 0x57) // LD D,A
    z80.deposit(0x000A, 0x7B) // LD A,E
    z80.deposit(0x000B, 0x00) // NOP
    z80.deposit(0x000C, 0xCD) // CALL nnnn
    z80.deposit(0x000D, 0x00)
    z80.deposit(0x000E, 0x01) // CALL 0100
    z80.deposit(0x000F, 0xB2) // OR D
    z80.deposit(0x0010, 0x76) // HALT

    z80.deposit(0x0100, 0x00) // NOP
    z80.deposit(0x0101, 0xD6) // SUB nn
    z80.deposit(0x0102, 0x30) // '0'
    z80.deposit(0x0103, 0xFE) // CP 10
    z80.deposit(0x0104, 10)
    z80.deposit(0x0105, 0xD8) // RET C
    z80.deposit(0x0106, 0x00) // NOP
    z80.deposit(0x0107, 0xD6) // SUB nn
    z80.deposit(0x0108, 0x07) // 'A' - '0' - 10
    z80.deposit(0x0109, 0xC9) // RET

    z80.PC(0x0000)
    z80.SP(0x0200) // Set up a stack
    z80.DE(0x3031) // 01
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0010)
    assertTrue(z80.A.get8 == 0x01)

    z80.PC(0x0000)
    z80.SP(0x0200) // Set up a stack
    z80.DE(0x3135) // 15
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0010)
    assertTrue(z80.A.get8 == 0x15)

  }

  @Test
  def test0x1f(): Unit = {
    z80.deposit(0x0000, 0x1f) // RRA
    z80.deposit(0x0001, 0x76) // HALT

    z80.PC(0x0000)
    z80.A(0x02)
    z80.F(z80.FLAG_Z) // turn on Z

    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0001)
    //The Carry becomes the bit leaving on the right, H, N flags are reset, P/V , S, and Z are preserved.
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_N))
    assertTrue(z80.A.get8 == 0x01)

    z80.PC(0x0000)
    z80.runcpu()
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_N))
    assertTrue(z80.A.get8 == 0x00)

  }

  @Test
  def test0xf6(): Unit = {

    z80.deposit(0x0000, 0xF6) // or $F0
    z80.deposit(0x0001, 0xF0)
    z80.deposit(0x0002, 0x76) // HLT

    z80.PC(0x0000)
    z80.A(0x01)
    z80.runcpu()
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(z80.A.get8 == 0xF1)

  }

  @Test
  def test0x27(): Unit = {
    z80.deposit(0x0000, 0x27) // DAA
    z80.deposit(0x0001, 0x76) // HALT

    z80.PC(0x0000)
    z80.A(0xA1)
    z80.F(z80.FLAG_N) // turn on N
    z80.runcpu()

    assertTrue(z80.PC.get16 == 0x0001)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_N)) // N preserved


  }

  @Test
  //The following routine converts the input to the ascii values that represent
  // the values 0 through to F. EG if a = 255 then d =70 and e = 70 as “F” is ascii value 70.
  def testToASCII(): Unit = {
    z80.deposit(0x0000, 0x4f) // LD C,A
    z80.deposit(0x0001, 0xCD) // CALL nnnn
    z80.deposit(0x0002, 0x00)
    z80.deposit(0x0003, 0x01) // CALL 0100 num1
    z80.deposit(0x0004, 0x57) // LD D,A
    z80.deposit(0x0005, 0x79) // LD A,C
    z80.deposit(0x0006, 0x00) // NOP
    z80.deposit(0x0007, 0xCD) // CALL nnnn
    z80.deposit(0x0008, 0x04)
    z80.deposit(0x0009, 0x01) // CALL 0104 num2
    z80.deposit(0x000A, 0x5F) // LD E,A
    z80.deposit(0x000B, 0x76) // HLT

    z80.deposit(0x0100, 0x1f) // RRA
    z80.deposit(0x0101, 0x1f) // RRA
    z80.deposit(0x0102, 0x1f) // RRA
    z80.deposit(0x0103, 0x1f) // RRA
    z80.deposit(0x0104, 0xF6) // or $F0
    z80.deposit(0x0105, 0xF0)
    z80.deposit(0x0106, 0x27) // DAA
    z80.deposit(0x0107, 0xC6) // ADD A,$A0
    z80.deposit(0x0108, 0xA0)
    z80.deposit(0x0109, 0xCE) // ADC A,$40
    z80.deposit(0x010a, 0x40)
    z80.deposit(0x010b, 0xC9) // RET

    z80.PC(0x0000)
    z80.A(255)
    z80.SP(0x120) // Set up a stack
    z80.runcpu()


    assertTrue(z80.PC.get16 == 0x000B)
    // TODO DAA is still not right.

  }

  @Test
  def testBlockMove(): Unit = {
    // Initial setup
    var loc = 0x0000
    z80.PC(loc)

    z80.deposit(loc, 0x03) // inc BC
    loc += 1
    z80.deposit(loc, 0x03) // inc BC
    loc += 1
    z80.deposit(loc, 0xED) // ldir
    loc += 1
    z80.deposit(loc, 0xB0) //
    loc += 1
    z80.deposit(loc, 0x03) // inc BC
    loc += 1
    z80.deposit(loc, 0x03) // inc BC
    loc += 1
    z80.deposit(loc, 0xED) // lddr
    loc += 1
    z80.deposit(loc, 0xB8) //
    loc += 1
    z80.deposit(loc, 0x76) // halt

    z80.runcpu()

  }

  @Test
  def testDasm(): Unit = {
    z80.deposit(0x0000, 0x4f) // LD C,A
    z80.deposit(0x0001, 0xCD) // CALL nnnn
    z80.deposit(0x0002, 0x00)
    z80.deposit(0x0003, 0x01) // CALL 0100 num1
    z80.deposit(0x0004, 0x57) // LD D,A
    z80.deposit(0x0005, 0x79) // LD A,C
    z80.deposit(0x0006, 0x00) // NOP
    z80.deposit(0x0007, 0xCD) // CALL nnnn
    z80.deposit(0x0008, 0x04)
    z80.deposit(0x0009, 0x01) // CALL 0104 num2
    z80.deposit(0x000A, 0x5F) // LD E,A
    z80.deposit(0x000B, 0x76) // HLT

    z80.deposit(0x0100, 0x1f) // RRA
    z80.deposit(0x0101, 0x1f) // RRA
    z80.deposit(0x0102, 0x1f) // RRA
    z80.deposit(0x0103, 0x1f) // RRA
    z80.deposit(0x0104, 0xF6) // or $F0
    z80.deposit(0x0105, 0xF0)
    z80.deposit(0x0106, 0x27) // DAA
    z80.deposit(0x0107, 0xC6) // ADD A,$A0
    z80.deposit(0x0108, 0xA0)
    z80.deposit(0x0109, 0xCE) // ADC A,$40
    z80.deposit(0x010a, 0x40)
    z80.deposit(0x010b, 0xC9) // RET

    val sb = new StringBuilder
    assertTrue({
      z80.DAsm(0x0000, sb)
      sb.toString == "LD C,A"
    })
    sb.clear()
    assertTrue({
      z80.DAsm(0x0107, sb)
      sb.toString() == "ADD A,a0h"
    })
    sb.clear()
    assertTrue({
      z80.DAsm(0x0007, sb)
      sb.toString() == "CALL 0104h"
    })
    sb.clear()
    assertTrue({
      z80.DAsm(0x0104, sb)
      sb.toString() == "OR f0h"
    })
    sb.clear()

    z80.DAsm(0x00, 0x10b, sb)
    Utils.outln(sb.toString)
  }

  @Test
  def text0xe6(): Unit = {

    z80.deposit(0x0000, 0xE6) // AND nn
    z80.deposit(0x0001, 0x40) // 40h
    z80.deposit(0x0002, 0x76) // HLT

    z80.PC(0x0000)
    z80.A(0x40)
    z80.F(0xff) // set all flags

    z80.runcpu()

    assertTrue(z80.A.intValue == 0x40)
    assertTrue((z80.F & z80.FLAG_Z) == 0)
    assertTrue((z80.F & z80.FLAG_S) == 0)
    assertTrue((z80.F & z80.FLAG_N) == 0) // N Flag is cleared
    assertTrue((z80.F & z80.FLAG_P) == 0)
    assertTrue((z80.F & z80.FLAG_H) != 0)
    assertTrue((z80.F & z80.FLAG_C) == 0) // C flag is cleared

    z80.PC(0x0000)
    //32 + 16
    z80.A(0x20 + 0x10)

    z80.runcpu()

    assertTrue(z80.A.intValue == 0x00)
    assertTrue((z80.F & z80.FLAG_Z) != 0) // Z Flag is set.
    assertTrue((z80.F & z80.FLAG_S) == 0)
    assertTrue((z80.F & z80.FLAG_N) == 0)
    assertTrue((z80.F & z80.FLAG_P) != 0)
    assertTrue((z80.F & z80.FLAG_H) != 0)
  }

  @Test
  def testPushPop(): Unit = {

    z80.SP(0x120) // Set up a stack
    z80.PC(0x0000)
    z80.F(0x0000) // Flags zero'd

    // PUSH DE is 0xd5
    // POP DE is 0xd1
    // LD DE, nnnn is 0x11 nnnn
    z80.deposit(0x0000, 0x11) // LD DE,nnnn
    z80.depositWord(0x0001, 0x4567)
    z80.deposit(0x0003, 0xd5) // PUSH DE
    z80.deposit(0x0004, 0x76)
    z80.deposit(0x0005, 0x11) // LD DE, nnnnn
    z80.depositWord(0x0006, 0x1234)
    z80.deposit(0x0008, 0xd1) // POP DE
    z80.deposit(0x0009, 0x76) // HLT

    // Run first half
    z80.runcpu()
    // Make sure stack is where it should be
    assertTrue(z80.SP.intValue == 0x11E)
    assertTrue(z80.DE.intValue == 0x4567)

    // Flags are unaffected
    assertTrue(z80.F.intValue == 0x00)

    // run second half
    z80.PC(0x0005)
    z80.runcpu()

    assertTrue(z80.SP.intValue == 0x120)
    assertTrue(z80.DE.intValue == 0x4567)

    // Flags are unaffected
    assertTrue(z80.F.intValue == 0x00)

  }

  @Test
  def test0xb8(): Unit = {
    // CP B = 0xb8
    /*
        Unsigned

          If A == N, then Z flag is set.
          If A != N, then Z flag is reset.
          If A < N, then C flag is set.
          If A >= N, then C flag is reset.

        Signed

          If A == N, then Z flag is set.
          If A != N, then Z flag is reset.
          If A < N, then S and P/V are different.
          A >= N, then S and P/V are the same.
        */
    z80.deposit(0x0000, 0xb8) // CP B
    z80.deposit(0x0001, 0x76) // HLT


    z80.PC(0x0000)
    z80.A(0x0A) // 10
    z80.B(0x05)

    z80.runcpu()
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.intValue == 0x0a)
    assertTrue(z80.B.intValue == 0x05)

    z80.PC(0x0000)
    z80.A(0x0A) // 10
    z80.B(0x0B) // 11
    z80.runcpu()
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.intValue == 0x0a)
    assertTrue(z80.B.intValue == 0x0B)

    z80.PC(0x0000)
    z80.B(0x0A)
    z80.runcpu()
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.A.intValue == 0x0a)
    assertTrue(z80.B.intValue == 0x0A)

  }

  @Test
  def test0x1d(): Unit = {
    // DEC E
    z80.deposit(0x0000, 0x1D) // DEC E
    z80.deposit(0x0001, 0x76) // HLT

    z80.PC(0x0000)
    z80.E(0x0A) // 10

    z80.runcpu()
    assertTrue(z80.E.intValue == 0x09)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.E(0x01)

    z80.runcpu()
    assertTrue(z80.E.intValue == 0x00)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.E(0x00)

    z80.runcpu()
    assertTrue(z80.E.intValue == 0xFF)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))

  }

  @Test
  def test0xc6(): Unit = {
    // ADD A,nn
    z80.deposit(0x0000, 0xc6) // ADD A,10
    z80.deposit(0x0001, 0x0A) // 10
    z80.deposit(0x0002, 0x76) // HLT

    z80.PC(0x0000)
    z80.AF(z80.FLAG_N) // zero flags, A zero, Flag N set
    z80.runcpu()

    assertTrue(z80.PC.intValue == 0x02)
    assertTrue(z80.A.intValue == 0x0A)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_N)) // N reset
    assertTrue(z80.F.intValue == 8) // All flags clear except unknown flag

    z80.PC(0x0000)
    z80.AF(0xFF00)
    z80.runcpu()

    assertTrue(z80.A.intValue == 0x09)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))

    z80.PC(0x0000)
    z80.AF(0xF600)
    z80.runcpu()

    assertTrue(z80.A.intValue == 0x00)
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))

  }

  @Test
  def test0xce(): Unit = {
    // ADC A,nn
    z80.deposit(0x0000, 0xce) // ADD A,10
    z80.deposit(0x0001, 0x0A) // 10
    z80.deposit(0x0002, 0x76) // HLT

    z80.PC(0x0000)
    z80.AF(z80.FLAG_N) // zero flags, A
    z80.runcpu()

    assertTrue(z80.PC.intValue == 0x02)
    assertTrue(z80.A.intValue == 0x0A)

  }

  @Test
  def test0x22(): Unit = {
    // LD (nnnn),HL
    z80.deposit(0x0000, 0x22) // LD (nnnn), HL
    z80.deposit(0x0001, 0x00)
    z80.deposit(0x0002, 0x01) // 0100
    z80.deposit(0x0003, 0x76) // HLT

    z80.PC(0x0000)
    z80.HL(0x1234)

    z80.runcpu()

    assertTrue(z80.examineWord(0x100).intValue == 0x1234)

  }

  @Test
  def test0xb7(): Unit = {
    // OR A
    z80.deposit(0x0000, 0xb7) // OR A
    z80.deposit(0x0001, 0x76) // HLT

    z80.PC(0x0000)
    z80.AF(0x0142)
    z80.runcpu()

    assertTrue(z80.A.intValue == 0x01) // Value should not change
    assertTrue(z80.F.intValue == 0x00) // no flags

  }

  @Test
  def testGeneralInstructions(): Unit = {
    var addr = 0xC000
    // EX AF AF
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x01) // LD BC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x12) //

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x34)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xC5) // PUSH BC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF1) // POP AF

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x08)
    z80.deposit(addr, 0x76) // HALT

    z80.resetCPU()
    z80.PC(0xc0000)
    z80.runcpu()
    assertEquals(0x00, z80.A.get8.intValue)
    assertEquals(0x00, z80.F.get8.intValue)
    assertEquals(0x34, z80.AP.get8.intValue)
    assertEquals(0x12, z80.FP.get8.intValue)
/*

    // DJNZ
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x01) // LD BC
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x04)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x0C) // INC C

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x10) // DJNZ
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFD) // -2

    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xd000)
    z80.runcpu()
    assertEquals(0x0004, z80.BC.get16.intValue)
*/
    // JR NZ
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x20)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x01) // 1

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C) // INC A

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3D) // DEC A

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xC0) // RET NZ

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x20)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x01)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()
    assertEquals(0x01, z80.A.get8.intValue)


    // JR NC
    addr = 0xC000
    z80.resetCPU()
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x37) // SCF

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x30) // JR NC
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x02) // 2

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xEE) // XOR 0xFF

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFF)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x30) // JR NC
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00) // 0

    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()
    assertEquals(0xFF, z80.A.get8.intValue)


    // JR C
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x38)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x02)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xEE)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFF)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x37)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x38)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x0D) // DEC C

    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xd000)
    z80.runcpu()
    assertEquals(0xFF, z80.A.get8.intValue)
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xCD) // CALL

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x01)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x34)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x12)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xC0)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x1234, z80.BC.get16.intValue)
    // CALL Z
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E) // LD A

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFF)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xCC)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xCC)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit(addr, 0xC9) // RET

    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x01, z80.A.get8.intValue)

/*
    // RET NC / JP NC / CALL NC
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x37)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD4) // CALL NC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD2) // JP NC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3F) // CCF

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD4)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFF)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD2)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0) // RET NC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x02, z80.A.get8)
    */
    // RET C / JP C
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x37)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xCD)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xDA) // JP C

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD8) // RET C

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x88, z80.A.get8.intValue)
    // EXX / EX (SP),HL
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x03) // INC BC

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x13) // INC DE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x13)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x23) // INC HL

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x23)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x23)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD9) // EXX

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x11) // LD DE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x34)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x12)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD5) // PUSH DE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE3)
    z80.deposit(addr, 0x76)
    z80.resetCPU()
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x0001, z80.BCP.get16.intValue)
    assertEquals(0x0002, z80.DEP.get16.intValue)
    assertEquals(0x0003, z80.HP.get8.intValue)
    assertEquals(0x1234, z80.HL.get16.intValue)
    // IN A
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x12)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD3) // OUT A

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x34)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xDB)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x12, z80.A)
    // RET PO / JP PO / CALL PO
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x7F)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE4) // CALL PO

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0) // RET PO

    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE2) // JP PO

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3D)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3D)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE4)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFF)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE2)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x02, z80.A)
    // RET PE / JP PE / CALL PE
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x7F)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xEC) // CALL PE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x7F)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xEA) // JP PE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0) // RET PE

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE8)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x01, z80.A.get8)
    // JMP (HL)
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x33)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x21) // LD HL

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE9)
    z80.deposit(addr, 0xD0)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0xD0)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x77, z80.A)
    // LD SP,HL
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x21)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x34)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x12)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF9)
    z80.deposit(addr, 0xD0)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x1236, z80.SP)
    // RET NS / JP NS / CALL NS
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x80)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF4) // CALL NS

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF2) // JP NS

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x08)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF4)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x43)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF2)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF0) // RET NS

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x07F)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF0)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x80, z80.A)
    // RET S / JP S / CALL S
    addr = 0xC000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x7F)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFC) // CALL S

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xD0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xAA)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xFA) // JP S

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x00)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xE0)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x88)
    z80.deposit(addr, 0x76)
    addr = 0xD000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF8) // RET S

    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x77)
    z80.deposit(addr, 0x76)
    addr = 0xE000
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3E)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x11)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0x3C)
    z80.deposit({
      addr += 1
      addr - 1
    }, 0xF8)
    z80.deposit(addr, 0x76)
    z80.resetCPU
    z80.PC(0xc000)
    z80.runcpu()

    assertEquals(0x12, z80.A.get8)
  }


  //@Test
  def testBasicCPU(): Unit = {

    z80.resetCPU()
    // A very simple I/O routine to simulate NAS-SYS character output call
    z80.deposit(0x30, 0xd3)
    z80.deposit(0x31, 0x00)
    z80.deposit(0x32, 0xc9)

    val utils = new TestUtils
    utils.readHexDumpFile("nastest.hex",z80)
    z80.PC(0x1000)

    z80.runcpu()

  }


}

object Z80Tests {
  var z80: Z80 = null
  var mmu: BasicMMU = null
  var machine: S100Machine = null
  var PC: Register16 = null
  val HALT = 0x76
}