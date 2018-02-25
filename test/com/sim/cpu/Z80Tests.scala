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
      Z80Tests.mmu.mapRAM(UInt(0x0000), UInt(0x500))
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
    assertTrue((z80.F.get8 & 1) == 0) // Carry flag cleared

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
    //assertFalse(z80.testFlag(z80.F, z80.FLAG_Z)) // Not sure about this one.
    assertFalse(z80.testFlag(z80.F, z80.FLAG_P))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_N))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))


  }

  @Test
  // JR NZ
  def test0x20() :Unit = {
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
    assertTrue(z80.testFlag(z80.F, z80.FLAG_Z))
    assertTrue(z80.testFlag(z80.F, z80.FLAG_C))

    z80.PC(0x0000)
    z80.A(0x20)
    z80.runcpu()
    assertTrue(z80.PC.get16 == 0x0002)
    assertTrue(z80.A.get8 == 0x20)
    assertFalse(z80.testFlag(z80.F, z80.FLAG_Z))
    assertFalse(z80.testFlag(z80.F, z80.FLAG_C))


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


  }

  @Test
  def testBlockMove(): Unit = {
    // Initial setup
    var loc = 0x0000
    z80.PC(loc)

    z80.deposit(loc, 0x03); // inc BC
    loc += 1
    z80.deposit(loc, 0x03); // inc BC
    loc += 1
    z80.deposit(loc, 0xED); // ldir
    loc += 1
    z80.deposit(loc, 0xB0); //
    loc += 1
    z80.deposit(loc, 0x03); // inc BC
    loc += 1
    z80.deposit(loc, 0x03); // inc BC
    loc += 1
    z80.deposit(loc, 0xED); // lddr
    loc += 1
    z80.deposit(loc, 0xB8); //
    loc += 1
    z80.deposit(loc, 0x76); // halt

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