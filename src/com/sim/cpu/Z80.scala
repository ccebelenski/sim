package com.sim.cpu

import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}
import com.sim.{SimTimer, Utils}

import scala.annotation.switch

class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "Z80"
  override val MMU: BasicMMU = new Z80MMU(this)

  override def init(): Unit = ???

  var tStates: Long = 0L

  override def createDefaultUnitOptions: Unit = {}

  override def resetCPU(): Unit = {

    tStates = 0L
    PC(0x0000)
    HL(0x0000)
    BC(0x0000)
    AF(0x0000)
    DE(0x0000)
    IX(0x0000)
    IY(0x0000)
    SP(0x0000)
    AF(0x0000)
    R(0x0000)
    I(0x00)
    HLP(0x0000)
    BCP(0x0000)
    AFP(0x0000)
    DEP(0x0000)
    AFP(0x0000)

  }

  def showCommand(stringBuilder: StringBuilder) = {
    // TODO device specific stuff, probably registers, etc.

  }

  val H = new Register8("H")
  val L = new Register8("L")
  val A = new Register8("A")
  val F = new Register8("F")
  val B = new Register8("B")
  val C = new Register8("C")
  val D = new Register8("D")
  val E = new Register8("E")
  val I = new Register8("I")
  val HP = new Register8("H'")
  val LP = new Register8("L'")
  val AP = new Register8("A'")
  val FP = new Register8("F'")
  val BP = new Register8("B'")
  val CP = new Register8("C'")
  val DP = new Register8("D'")
  val EP = new Register8("E'")
  val R = new Register8("R")
  val HL = new CompositeRegister16("HL", H, L)
  val AF = new CompositeRegister16("AF", A, F)
  val BC = new CompositeRegister16("BC", B, C)
  val DE = new CompositeRegister16("DE", D, E)
  val HLP = new CompositeRegister16("HL'", HP, LP)
  val AFP = new CompositeRegister16("AF'", AP, FP)
  val BCP = new CompositeRegister16("BC'", BP, CP)
  val DEP = new CompositeRegister16("DE'", DP, EP)
  val IXH = new Register8("IXH") // Not real registers, but helpful to address as high/low
  val IXL = new Register8("IXL")
  val IYH = new Register8("IYH")
  val IYL = new Register8("IYL")
  val IX = new CompositeRegister16("IX", IXH, IXL)
  val IY = new CompositeRegister16("IY", IYH, IYL)
  val SP = new Register16("SP")
  val PC = new Register16("PC")

  override val registers: Map[String, Register] = Map("H" -> H, "L" -> L, "HL" -> HL,
    "B" -> B, "C" -> C, "BC" -> BC,
    "D" -> D, "E" -> E, "DE" -> DE,
    "A" -> A, "F" -> F, "AF" -> AF,
    "I" -> I, "IX" -> IX, "IY" -> IY,
    "SP" -> SP, "PC" -> PC, "R" -> R,
    "HP" -> HP, "LP" -> LP, "HLP" -> HLP,
    "BP" -> BP, "CP" -> CP, "BCP" -> BCP,
    "DP" -> DP, "EP" -> EP, "DEP" -> DEP,
    "AP" -> AP, "FP" -> FP, "AFP" -> AFP
  )

  resetCPU()

  override def showRegisters(): String = {
    s"$PC  $SP  $AF  $BC  $DE  $HL  $IX  $IY  $R\n                     $AFP $BCP $DEP $HLP"
  }

  val FLAG_C = 1
  val FLAG_N = 2
  val FLAG_P = 4
  val FLAG_H = 16
  val FLAG_Z = 64
  val FLAG_S = 128

  override def showFlags(): String = {
    val carry = if ((F & FLAG_C) != 0) true else false
    val addsub = if ((AF & FLAG_N) != 0) true else false
    val pv = if ((AF & FLAG_P) != 0) true else false
    val bit3 = if ((AF & 8) != 0) true else false
    val h = if ((AF & FLAG_H) != 0) true else false
    val bit5 = if ((AF & 32) != 0) true else false
    val z = if ((AF & FLAG_Z) != 0) true else false
    val s = if ((AF & FLAG_S) != 0) true else false
    val str = f"${F.toBinaryString}%8s".replaceAll(" ", "0")
    s"F=$str  :  S=$s  Z=$z  H=$h  P/V=$pv  N=$addsub  C=$carry"

  }


  override def runcpu(): Unit = {

    // tStates contains the number of t-states executed.  1 t-state is executed in 1 microsecond
    // on a 1Mhz CPU.  tStates is used for real-time simulations.
    val sliceLength = 10L
    var tStatesInSlice: Long = sliceLength * clockFrequency // Number of t-states in 10mSec time-slice
    var startTime: Long = System.currentTimeMillis()
    var now: Long = 0L
    var tStateModifier: Boolean = false
    var execute: Boolean = true
    var memoryBreak: Boolean = false

    while (execute) {
      if (SimTimer.sim_interval <= 0) { // Check clock queue
        // TODO sim_process_event()
        if (clockHasChanged) {
          clockHasChanged = false
          tStates = 0L
          startTime = System.currentTimeMillis()
          tStatesInSlice = sliceLength * clockFrequency


        }
        // TODO Interrupt stuff


        // Interrupted the sim
        if (com.sim.Console.userInterrupt) execute = false

      }

      // TODO Instruction execution

      INCR(1)
      val instr = MMU.get8(PC)
      PC.increment()

      (instr.intValue: @switch) match {

        case (0x00) => // NOP
          addTStates(4)
        case (0x01) => // LD BC, nnnn
          addTStates(10)
          BC(MMU.get16(PC))
          PC(PC + 2)
        case (0x02) => // LD (BC),A
          addTStates(7)
          memoryBreak = CHECK_BREAK_BYTE(BC)
          if (!memoryBreak) MMU.put8(BC, A)
        case (0x03) => // INC BC
          addTStates(6)
          BC.increment()
        case (0x04) => // INC B
          addTStates(4)
          B.increment()
          AF((AF & ~0xfe) | incTable(B) | SET_PV2(0x80, B))
        case (0x05) => // DEC B
          addTStates(4)
          B.decrement()
          AF((AF & ~0xfe) | decTable(B) | SET_PV2(0x7f, B))
        case (0x06) => // LD B,nn
          addTStates(7)
          B(MMU.get8(PC))
          PC.increment()
        case (0x07) => // RLCA
          addTStates(4)
          AF(((AF >> 7) & 0x0128) | ((AF << 1) & ~0x1ff) | (AF & 0xc4) | ((AF >> 15) & 1))
        case (0x08) => // EX AF, AF'
          addTStates(4)
          AF.swap(AFP)
        case (0x09) => // ADD HL, BC
          addTStates(11)
          // NB The table is based on the raw int result values, not the 16 bit values
          val sum: Int = HL.intValue + BC.intValue
          AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((HL ^ BC ^ sum) >> 8))
          HL(sum)
        case (0x0a) => // LD A, (BC)
          addTStates(7)
          memoryBreak = CHECK_BREAK_BYTE(BC)
          if (!memoryBreak) A(MMU.get8(BC))
        case (0x0b) => // DEC BC
          addTStates(6)
          BC.decrement()
        case (0x0c) => // INC C
          addTStates(4)
          C.increment()
          AF((AF & ~0xfe) | incTable(C) | SET_PV2(0x80, C))
        case (0x0d) => // DEC C
          addTStates(4)
          C.decrement()
          AF((AF & ~0xfe) | decTable(C) | SET_PV2(0x7f, C))
        case (0x0e) => // LD C,nn
          addTStates(7)
          C(MMU.get8(PC))
          PC.increment()
        case (0x0f) => // RRCA
          addTStates(4)
          AF((AF & 0xc4) | rrcaTable(A))
        case (0x10) => // DJNZ dd
          B.decrement()
          if (B.get8 == 0) {
            // Jump
            addTStates(13)
            PC(PC + MMU.get8(PC).byteValue + 1)
          } else {
            PC.increment()
            addTStates(8)
          }
        case (0x11) => // LD DE, nnnn
          addTStates(10)
          DE(MMU.get16(PC))
          PC(PC + 2)
        case (0x12) => // LD (DE),A
          addTStates(7)
          memoryBreak = CHECK_BREAK_BYTE(DE)
          if (!memoryBreak) MMU.put8(DE, A)
        case (0x13) => // INC DE
          addTStates(6)
          DE.increment()
        case (0x14) => // INC D
          addTStates(4)
          D.increment()
          AF((AF & ~0xfe) | incTable(D) | SET_PV2(0x80, D))
        case (0x15) => // DEC D
          addTStates(4)
          D.decrement()
          AF((AF & ~0xfe) | decTable(D) | SET_PV2(0x7f, D))
        case (0x16) => // LD D,nn
          addTStates(7)
          D(MMU.get8(PC))
          PC.increment()
        case (0x17) => // RLA
          addTStates(4)
          AF(((AF << 8) & 0x0100) | ((AF >> 7) & 0x28) | ((AF << 1) & ~0x01ff) |
            (AF & 0xc4) | ((AF >> 15) & 1))
        case (0x18) => // JR dd
          addTStates(12)
          PC(PC + MMU.get8(PC) + 1)
        case (0x19) => // ADD HL, DE
          addTStates(11)
          // NB The table is based on the raw int result values, not the 16 bit values
          val sum: Int = HL.intValue + DE.intValue
          AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((HL ^ DE ^ sum) >> 8))
          HL(sum)
        case (0x1a) => // LD A, (DE)
          addTStates(7)
          memoryBreak = CHECK_BREAK_BYTE(DE)
          if (!memoryBreak) A(MMU.get8(DE))
        case (0x1b) => // DEC DE
          addTStates(6)
          DE.decrement()
        case (0x1c) => // INC E
          addTStates(4)
          E.increment()
          AF((AF & ~0xfe) | incTable(E) | SET_PV2(0x80, E))
        case (0x1d) => // DEC E
          addTStates(4)
          E.decrement()
          AF((AF & ~0xfe) | decTable(E) | SET_PV2(0x7f, E))
        case (0x1e) => // LD E,nn
          addTStates(7)
          E(MMU.get8(PC))
          PC.increment()
        case (0x1f) => // RRA
          addTStates(4)
          AF(((AF & 1) << 15) | (AF & 0xc4) | rraTable(A))
        case (0x20) => // JR NZ,dd
          if ((F & 64) != 0) { // Z flag
            addTStates(7)
            PC.increment()
          } else {
            PC(PC + MMU.get8(PC) + 1)
            addTStates(12)
          }
        case (0x21) => // LD HL, nnnn
          addTStates(10)
          HL(MMU.get16(PC))
          PC(PC + 2)
        case (0x22) => // LD (nnnn), HL
          addTStates(16)
          memoryBreak = CHECK_BREAK_WORD(PC)
          if (!memoryBreak) MMU.put16(PC, HL)
          PC(PC + 2)
        case (0x23) => // INC HL
          addTStates(6)
          HL.increment()
        case (0x24) => // INC H
          addTStates(4)
          H.increment()
          AF((AF & ~0xfe) | incTable(H) | SET_PV2(0x80, H))
        case (0x25) => // DEC H
          addTStates(4)
          H.decrement()
          AF((AF & ~0xfe) | decTable(H) | SET_PV2(0x7f, H))
        case (0x26) => // LD H,nn
          addTStates(7)
          H(MMU.get8(PC))
          PC.increment()
        case (0x27) => // DAA
          addTStates(4)
          var acu = A.get8.intValue
          val temp = acu & 0xf // low digit
        val cbits = testFlag(F, FLAG_C) // carry
          if (testFlag(F, FLAG_N)) {
            // last operation was a subtract
            val hd = if (cbits || (acu > 0x99)) true else false
            // H = 16
            if (testFlag(F, FLAG_H) || (temp > 9)) {
              // adjust low digit
              if (temp > 5) setFlag(F, FLAG_H, clear = false)
              acu = (acu - 6) & 0xff
            }
            if (hd) acu = acu - 0x160 // adjust high digit

          } else {
            // last operation was an add
            if (testFlag(F, FLAG_H) || (temp > 9)) {
              if (temp > 9) setFlag(F, FLAG_H, clear = {
                if (temp > 9) true else false
              })
              acu = acu + 6
            }
            if (cbits || ((acu & 0x1f0) > 0x90))
              acu = acu + 0x60 // adjust high bit
          }
          AF((AF & 0x12) | rrdrldTable(acu & 0xff) | ((acu >> 8) & 1) | {
            if (cbits) 1 else 0
          })
        case (0x28) => // JR Z,dd
          if (testFlag(F, FLAG_Z)) {
            addTStates(12)
            PC(PC + MMU.get8(PC) + 1)
          } else {
            PC.increment()
            addTStates(7)
          }
        case (0x29) => // ADD HL, HL
          addTStates(11)
          // NB The table is based on the raw int result values, not the 16 bit values
          val sum: Int = HL.intValue + HL.intValue
          AF((AF & ~0x3b) | cbitsDup16Table(sum >> 8))
          HL(sum)
        case (0x2a) => // LD HL,(nnnn)
          addTStates(16)
          CHECK_BREAK_WORD(PC)
          HL(MMU.get16(MMU.get16(PC)))
          PC(PC + 2)
        case (0x2b) => // DEC HL
          addTStates(6)
          HL.decrement()
        case (0x2c) => // INC L
          addTStates(4)
          L.increment()
          AF((AF & ~0xfe) | incTable(L) | SET_PV2(0x80, L))
        case (0x2d) => // DEC L
          addTStates(4)
          L.decrement()
          AF((AF & ~0xfe) | decTable(L) | SET_PV2(0x7f, L))
        case (0x2e) => // LD L,nn
          addTStates(7)
          L(MMU.get8(PC))
          PC.increment()
        case (0x2f) => // CPL
          addTStates(4)
          AF((~AF.get16 & ~0xff) | (AF & 0xc5) | ((~AF.get16 >> 8) & 0x28) | 0x12)
        case (0x30) => // JR NC,dd
          if (!testFlag(F, FLAG_C)) {
            addTStates(12)
            PC(PC + MMU.get8(PC) + 1)
          } else {
            PC.increment()
            addTStates(7)
          }
        case (0x31) => // LD SP, nnnn
          addTStates(10)
          SP(MMU.get16(PC))
          PC(PC + 2)
        case (0x32) => // LD (nnnn), A
          addTStates(13)
          memoryBreak = CHECK_BREAK_WORD(PC)
          MMU.put8(MMU.get16(PC).intValue, A)
          PC(PC + 2)
        case (0x33) => // INC SP
          addTStates(6)
          SP.increment()
        case (0x34) => // INC (HL)
          addTStates(11)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL) + 1
          MMU.put8(HL, UByte(temp.byteValue()))
          AF((AF & ~0xfe) | incTable(temp) | SET_PV2(0x80, temp))
        case (0x35) => // DEC (HL)
          addTStates(11)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL) - 1
          MMU.put8(HL, UByte(temp.byteValue()))
          AF((AF & ~0xfe) | decTable(temp) | SET_PV2(0x7f, temp))
        case (0x36) => // LD (HL),nn
          addTStates(10)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, MMU.get8(PC))
          PC.increment()
        case (0x37) => // SCF
          addTStates(4)
          AF((AF & ~0x3b) | ((AF >> 8) & 0x28) | 1)
        case (0x38) => // JR C,dd
          if (testFlag(F, FLAG_C)) {
            addTStates(12)
            PC(PC + MMU.get8(PC) + 1)
          } else {
            PC.increment()
            addTStates(7)
          }
        case (0x39) => // ADD HL, SP
          addTStates(11)
          // NB The table is based on the raw int result values, not the 16 bit values
          val sum: Int = HL.intValue + SP.intValue
          AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((HL ^ SP ^ sum) >> 8))
          HL(sum)
        case (0x3a) => // LD A,(nnnn)
          addTStates(13)
          val tmp: UShort = MMU.get16(PC)
          CHECK_BREAK_BYTE(tmp)
          A(MMU.get8(tmp))
          PC(PC + 2)
        case (0x3b) => // DEC SP
          addTStates(6)
          SP.decrement()
        case (0x3c) => // INC A
          addTStates(4)
          A.increment()
          AF((AF & ~0xfe) | incTable(A) | SET_PV2(0x80, A))
        case (0x3d) => // DEC A
          addTStates(4)
          A.decrement()
          AF((AF & ~0xfe) | decTable(A) | SET_PV2(0x7f, A))
        case (0x3e) => // LD A,nn
          addTStates(7)
          A(MMU.get8(PC))
          PC.increment()
        case (0x3f) => // CCF
          addTStates(4)
          AF((AF & ~0x3b) | ((AF >> 8) & 0x28) | ((AF & 1) << 4) | (~AF.get16 & 1))
        case (0x40) => // LD B,B
          addTStates(4)
        case (0x41) => // LD B,C
          addTStates(4)
          B(C)
        case (0x42) => // LD B,D
          addTStates(4)
          B(D)
        case (0x43) => // LD B,E
          addTStates(4)
          B(E)
        case (0x44) => // LD B,H
          addTStates(4)
          B(H)
        case (0x45) => // LD B,L
          addTStates(4)
          B(L)
        case (0x46) => // LD B,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          B(MMU.get8(HL))
        case (0x47) => // LD B,A
          addTStates(4)
          B(A)
        case (0x48) => // LD C,B
          addTStates(4)
          C(B)
        case (0x49) => // LD C,C
          addTStates(4)
        case (0x4a) => // LD C,D
          addTStates(4)
          C(D)
        case (0x4b) => // LD C,E
          addTStates(4)
          C(E)
        case (0x4c) => // LD C,H
          addTStates(4)
          C(H)
        case (0x4d) => // LD C,L
          addTStates(4)
          C(L)
        case (0x4e) => // LD C,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          C(MMU.get8(HL))

        case (0x4f) => // LD C,A
          addTStates(4)
          C(A)
        case (0x50) => // LD D,B
          addTStates(4)
          D(B)
        case (0x51) => // LD D,C
          addTStates(4)
          D(C)
        case (0x52) => // LD D,D
          addTStates(4)
        case (0x53) => // LD D,E
          addTStates(4)
          D(E)
        case (0x54) => // LD D,H
          addTStates(4)
          D(H)
        case (0x55) => // LD D,L
          addTStates(4)
          D(L)
        case (0x56) => // LD D,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          D(MMU.get8(HL))
        case (0x57) => // LD D,A
          addTStates(4)
          D(A)

        case (0x58) => // LD E,B
          addTStates(4)
          E(B)
        case (0x59) => // LD E,C
          addTStates(4)
          E(C)
        case (0x5a) => // LD E,D
          addTStates(4)
          E(D)
        case (0x5b) => // LD E,E
          addTStates(4)
        case (0x5c) => // LD E,H
          addTStates(4)
          E(H)
        case (0x5d) => // LD E,L
          addTStates(4)
          E(L)
        case (0x5e) => // LD E,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          E(MMU.get8(HL))
        case (0x5f) => // LD E,A
          addTStates(4)
          E(A)

        case (0x60) => // LD H,B
          addTStates(4)
          H(B)
        case (0x61) => // LD H,C
          addTStates(4)
          H(C)
        case (0x62) => // LD H,D
          addTStates(4)
          H(D)
        case (0x63) => // LD H,E
          addTStates(4)
          H(E)
        case (0x64) => // LD H,H
          addTStates(4)
        case (0x65) => // LD H,L
          addTStates(4)
          H(L)
        case (0x66) => // LD H,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          H(MMU.get8(HL))
        case (0x67) => // LD H,A
          addTStates(4)
          H(A)

        case (0x68) => // LD L,B
          addTStates(4)
          L(B)
        case (0x69) => // LD L,C
          addTStates(4)
          L(C)
        case (0x6a) => // LD L,D
          addTStates(4)
          L(D)
        case (0x6b) => // LD L,E
          addTStates(4)
          L(E)
        case (0x6c) => // LD L,H
          addTStates(4)
          L(H)
        case (0x6d) => // LD L,L
          addTStates(4)
        case (0x6e) => // LD L,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          L(MMU.get8(HL))
        case (0x6f) => // LD L,A
          addTStates(4)
          L(A)

        case (0x70) => // LD (HL),B
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, B)
        case (0x71) => // LD (HL),C
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, C)
        case (0x72) => // LD (HL),D
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, D)
        case (0x73) => // LD (HL),E
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, E)
        case (0x74) => // LD (HL),H
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, H)
        case (0x75) => // LD (HL),L
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, L)


        case (0x76) => // HALT
          addTStates(4)
          PC(PC - 1)
          // TODO Check stop on halt, otherwise sim_sleep
          execute = false


        case (0x77) => // LD (HL),A
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          MMU.put8(HL, A)

        case (0x78) => // LD A,B
          addTStates(4)
          A(B)
        case (0x79) => // LD A,C
          addTStates(4)
          A(C)
        case (0x7a) => // LD A,D
          addTStates(4)
          A(D)
        case (0x7b) => // LD A,E
          addTStates(4)
          A(E)
        case (0x7c) => // LD A,H
          addTStates(4)
          A(H)
        case (0x7d) => // LD A,L
          addTStates(4)
          A(L)
        case (0x7e) => // LD A,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          A(MMU.get8(HL))
        case (0x7f) => // LD A,A
          addTStates(4)

        case (0x80) => // ADD A,B
          addTStates(4)
          val temp = B.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x81) => // ADD A,C
          addTStates(4)
          val temp = C.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x82) => // ADD A,D
          addTStates(4)
          val temp = D.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x83) => // ADD A,E
          addTStates(4)
          val temp = E.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x84) => // ADD A,H
          addTStates(4)
          val temp = H.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x85) => // ADD A,L
          addTStates(4)
          val temp = L.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x86) => // ADD A,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL)
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x87) => // ADD A,A
          addTStates(4)
          val cbits: Int = 2 * A
          AF(cbitsDup8Table(cbits) | SET_PV(cbits))


        case (0x88) => // ADC A,B
          addTStates(4)
          val temp = B.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x89) => // ADC A,C
          addTStates(4)
          val temp = C.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8a) => // ADC A,D
          addTStates(4)
          val temp = D.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8b) => // ADC A,E
          addTStates(4)
          val temp = E.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8c) => // ADC A,H
          addTStates(4)
          val temp = H.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8d) => // ADC A,L
          addTStates(4)
          val temp = L.get8
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8e) => // ADC A,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL)
          val acu = A.get8
          val sum: Int = acu.intValue + temp.intValue + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0x8f) => // ADC A,A
          addTStates(4)
          val cbits = 2 * A.get8 + {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          AF(cbitsDup8Table(cbits) | SET_PV(cbits))

        case (0x90) => // SUB B
          addTStates(4)
          val temp = B.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x91) => // SUB C
          addTStates(4)
          val temp = C.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x92) => // SUB D
          addTStates(4)
          val temp = D.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x93) => // SUB E
          addTStates(4)
          val temp = E.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x94) => // SUB H
          addTStates(4)
          val temp = H.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x95) => // SUB L
          addTStates(4)
          val temp = L.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x96) => // SUB (HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL)
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x97) => // SUB A
          addTStates(4)
          AF(0x42)

        case (0x98) => // SBC A,B
          addTStates(4)
          val temp = B.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x99) => // SBC A,C
          addTStates(4)
          val temp = C.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9a) => // SBC A,D
          addTStates(4)
          val temp = D.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9b) => // SBC A,E
          addTStates(4)
          val temp = E.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9c) => // SBC A,H
          addTStates(4)
          val temp = H.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9d) => // SBC A,L
          addTStates(4)
          val temp = L.get8
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9e) => // SBC A,(HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          val temp = MMU.get8(HL)
          val acu = A.get8
          val sum: Int = acu.intValue - temp.intValue - {
            if (testFlag(A, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0x9f) => // SBC A,A
          addTStates(4)
          val cbits = {
            if (testFlag(A, FLAG_C)) -1 else 0
          }
          AF(subTable(cbits & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))


        case (0xa0) => // AND B
          addTStates(4)
          AF(andTable(A & B))
        case (0xa1) => // AND C
          addTStates(4)
          AF(andTable(A & C))
        case (0xa2) => // AND D
          addTStates(4)
          AF(andTable(A & D))
        case (0xa3) => // AND E
          addTStates(4)
          AF(andTable(A & E))
        case (0xa4) => // AND H
          addTStates(4)
          AF(andTable(A & H))
        case (0xa5) => // AND L
          addTStates(4)
          AF(andTable(A & L))
        case (0xa6) => // AND (HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          AF(andTable(A & MMU.get8(HL).intValue))
        case (0xa7) => // AND A
          addTStates(4)
          AF(andTable(A))


        case (0xa8) => // XOR B
          addTStates(4)
          AF(xororTable(A ^ B))
        case (0xa9) => // XOR C
          addTStates(4)
          AF(xororTable(A ^ C))
        case (0xaa) => // XOR D
          addTStates(4)
          AF(xororTable(A ^ D))
        case (0xab) => // XOR E
          addTStates(4)
          AF(xororTable(A ^ E))
        case (0xac) => // XOR H
          addTStates(4)
          AF(xororTable(A ^ H))
        case (0xad) => // XOR L
          addTStates(4)
          AF(xororTable(A ^ L))
        case (0xae) => // XOR (HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          AF(xororTable(A ^ MMU.get8(HL).intValue))
        case (0xaf) => // XOR A
          addTStates(4)
          AF(0x44)

        case (0xb0) => // OR B
          addTStates(4)
          AF(xororTable(A | B))
        case (0xb1) => // OR C
          addTStates(4)
          AF(xororTable(A | C))
        case (0xb2) => // OR D
          addTStates(4)
          AF(xororTable(A | D))
        case (0xb3) => // OR E
          addTStates(4)
          AF(xororTable(A | E))
        case (0xb4) => // OR H
          addTStates(4)
          AF(xororTable(A | H))
        case (0xb5) => // OR L
          addTStates(4)
          AF(xororTable(A | L))
        case (0xb6) => // OR (HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          AF(xororTable(A | MMU.get8(HL).intValue))
        case (0xb7) => // OR A
          addTStates(4)
          AF(xororTable(A))


        case (0xb8) => // CP B
          addTStates(4)
          val temp: Int = B.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xb9) => // CP C
          addTStates(4)
          val temp: Int = C.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xba) => // CP D
          addTStates(4)
          val temp: Int = D.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xbb) => // CP E
          addTStates(4)
          val temp: Int = E.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xbc) => // CP H
          addTStates(4)
          val temp: Int = H.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xbd) => // CP L
          addTStates(4)
          val temp: Int = L.get8.intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xbe) => // CP (HL)
          addTStates(7)
          CHECK_BREAK_BYTE(HL)
          val temp: Int = MMU.get8(HL).intValue
          val acu: Int = A.get8.intValue
          AF(AF & ~0x28 | temp & 0x28)
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
        case (0xbf) => // CP A
          addTStates(4)
          F((A & 0x28) | 0x42)

        case (0xc0) => // RET NZ
          if (testFlag(F, FLAG_Z)) addTStates(5)
          else {
            CHECK_BREAK_WORD(SP)
            addTStates(11)
            POP(PC)
          }
        case (0xc1) => // POP BC
          addTStates(10)
          CHECK_BREAK_WORD(SP)
          POP(BC)


        case (0xc2) => // JP NZ,nnnn
          JPC(!testFlag(F, FLAG_Z))
        case (0xc3) => // JP nnnn
          JPC(true)
        case (0xc4) => // CALL NZ,nnnn
          CALLC(!testFlag(F, FLAG_Z))
        case (0xc5) => // PUSH BC
          addTStates(11)
          CHECK_BREAK_WORD(SP - 2)
          PUSH(BC)
        case (0xc6) => // ADD A,nn
          addTStates(7)
          val temp = MMU.get8(PC).intValue
          PC.increment()
          val acu = A.intValue
          val sum: Int = acu + temp
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0xc7) => // RST 0
          addTStates(11)
          CHECK_BREAK_WORD(SP - 2)
          PUSH(PC)
          PC(0x0000)
        case (0xc8) => // RET Z
          if (testFlag(F, FLAG_Z)) {
            CHECK_BREAK_WORD(SP)
            POP(PC)
            addTStates(11)
          } else {
            addTStates(5)
          }
        case (0xc9) => // RET
          addTStates(10)
          CHECK_BREAK_WORD(SP)
          POP(PC)
        case (0xca) => // JP Z,nnnn
          JPC(testFlag(F, FLAG_Z))
        case (0xcb) => // CB prefix
          INCR(1)
          val adr = HL.get16.intValue
          val op = MMU.get8(PC)
          var acu: Int = 0
          var cbits: Int = 0
          var temp: Int = 0
          tStateModifier = false


          (op & 7) match {
            case (0) =>
              PC.increment()
              acu = B
              addTStates(8)
            case (1) =>
              PC.increment()
              acu = C
              addTStates(8)
            case (2) =>
              PC.increment()
              acu = D
              addTStates(8)
            case (3) =>
              PC.increment()
              acu = E
              addTStates(8)
            case (4) =>
              PC.increment()
            case (5) =>
              PC.increment()
              acu = L
              addTStates(8)
            case (6) =>
              CHECK_BREAK_BYTE(adr)
              PC.increment()
              acu = MMU.get8(UInt(adr)).intValue
              addTStates(15)
              tStateModifier = true
            case (7) =>
              PC.increment()
              acu = A.intValue
              addTStates(8)
            case _ =>

          }
          (op & 0xc0) match {
            case (0x00) => // shift/rotate
              (op & 0x38) match {
                case (0x00) => //RLC
                  temp = (acu << 1) | (acu >> 7)
                  cbits = temp & 1
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x08) => // RRC
                  temp = (acu >> 1) | (acu << 7)
                  cbits = temp & 0x80
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x10) => // RL
                  temp = (acu >> 1) | {
                    if (testFlag(F, FLAG_C)) 1 else 0
                  }
                  cbits = acu & 0x80
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x18) => // RR
                  temp = (acu >> 1) | {
                    if (testFlag(F, FLAG_C)) 1 else 0
                  } << 7
                  cbits = acu & 1
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x20) => // SLA
                  temp = acu << 1
                  cbits = acu & 0x80
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x28) => // SRA
                  temp = (acu >> 1) | (acu & 0x80)
                  cbits = acu & 1
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x30) => // SLIA
                  temp = (acu << 1) | 1
                  cbits = acu & 0x80
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case (0x38) => // SRL
                  temp = acu >> 1
                  cbits = acu & 1
                  AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
                    if (cbits == 0) 0 else 1
                  })
                case _ =>
              }
            case (0x40) => // BIT
              if (tStateModifier) addTStates(-3)
              if ((acu & (1 << ((op >> 3) & 7))) != 0)
                AF((AF & ~0xfe) | 0x10 | ({
                  if ((op & 0x38) == 0x38)
                    1
                  else 0
                } << 7))
              else AF((AF & ~0xfe) | 0x54)
              if ((op & 7) != 6) AF(AF | (acu & 0x28))
              temp = acu
            case (0x80) => // RES
              temp = acu & ~(1 << ((op >> 3) & 7))
            case (0xc0) => // SET
              temp = acu | (1 << ((op >> 3) & 7))
            case _ =>
          }
          (op & 7) match {
            case (0) =>
              B(temp)
            case (1) =>
              C(temp)
            case (2) =>
              D(temp)
            case (3) =>
              E(temp)
            case (4) =>
              H(temp)
            case (5) =>
              L(temp)
            case (6) =>
              MMU.put8(adr, UByte(temp.byteValue()))
            case (7) =>
              A(temp)
            case _ =>

          }
        case (0xcc) => // CALL Z,nnnn
          CALLC(testFlag(F, FLAG_Z))
        case (0xcd) => // CALL nnnn
          CALLC(true)
        case (0xce) => // ADC A,nn
          addTStates(7)
          val temp = MMU.get8(PC)
          PC.increment()
          val acu = A.intValue
          val sum: Int = acu + temp.intValue + {
            if (testFlag(F, FLAG_C)) 1 else 0
          }
          val cbits: Int = acu ^ temp ^ sum
          AF(addTable(sum) | cbitsTable(cbits) | SET_PV(cbits))
        case (0xcf) => // RST 8
          addTStates(11)
          CHECK_BREAK_WORD(SP - 2)
          PUSH(PC)
          PC(0x0008)
        case (0xd0) => // RET NC
          if (testFlag(F, FLAG_C)) addTStates(5)
          else {
            CHECK_BREAK_WORD(SP)
            POP(PC)
            addTStates(11)
          }
        case (0xd1) => // POP DE
          addTStates(10)
          CHECK_BREAK_WORD(SP)
          POP(DE)
        case (0xd2) => // JP NC,nnnn
          JPC(!testFlag(F, FLAG_C))
        case (0xd3) => // OUT (nn),A
          addTStates(11)
          MMU.out8(MMU.get8(PC).intValue, A)
        case (0xd4) => // CALL NC, nnnn
          CALLC(!testFlag(F, FLAG_C))
        case (0xd5) => // PUSH DE
          addTStates(11)
          CHECK_BREAK_WORD(SP - 2)
          PUSH(DE)
        case (0xd6) => // SUB nn
          addTStates(7)
          val temp: Int = MMU.get8(PC).intValue
          PC.increment()
          val acu = A.intValue
          val sum: Int = acu - temp
          val cbits: Int = acu ^ temp ^ sum
          AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
        case (0xd7) => // RST 10H
          addTStates(11)
          CHECK_BREAK_WORD(SP - 2)
          PUSH(PC)
          PC(0x0010)
        case (0xd8) => // RET C
          if (testFlag(F, FLAG_C)) {
            CHECK_BREAK_WORD(SP)
            POP(PC)
            addTStates(11)
          } else addTStates(5)
        case (0xd9) => // EXX
          addTStates(4)
          var temp = BC.get16
          BC(BCP)
          BCP(temp)
          temp = DE.get16
          DE(DEP)
          DEP(temp)
          temp = HL.get16
          HL(HLP)
          HLP(temp)
        case (0xda) => // JP C,nnnn
          JPC(testFlag(F, FLAG_C))
        case (0xdb) => // IN A,(nn)
          addTStates(11)
          A(MMU.in8(MMU.get8(PC).intValue))
          PC.increment()
        case (0xdc) => // CALL C,nnnn
          CALLC(testFlag(F, FLAG_C))


        case (0xdd) => // DD Prefix
          INCR(1)
          val op: Int = MMU.get8(PC).intValue
          PC.increment()
          op match {
            case (0x09) => // ADD IX,BC
              addTStates(15)
              val sum: Int = IX.intValue + BC.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IX ^ BC ^ sum) >> 8))
              IX(sum)
            case (0x19) => // ADD IX,DE
              addTStates(15)
              val sum: Int = IX.intValue + DE.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IX ^ DE ^ sum) >> 8))
              IX(sum)

            case (0x21) => // LD IX,nnnn
              addTStates(14)
              IX(MMU.get16(PC))
              PC(PC + 2)
            case (0x22) => // LD (nnnn),IX
              addTStates(20)
              val temp: Int = MMU.get16(PC).intValue
              CHECK_BREAK_WORD(temp)
              MMU.put16(temp, IX)
              PC(PC + 2)

            case (0x23) => // INC IX
              addTStates(10)
              IX.increment()
            case (0x24) => // INC IXH
              addTStates(9)
              IXH.increment()
              AF((AF & ~0xfe) | incZ80Table(IXH))

            case (0x25) => // DEC IXH
              addTStates(9)
              IXH.decrement()
              AF((AF & ~0xfe) | decZ80Table(IXH))

            case (0x26) => // LD IXH,nn
              addTStates(9)
              IXH(MMU.get8(PC))
              PC.increment()
            case (0x29) => // ADD IX,IX
              addTStates(15)
              val sum: Int = IX + IX
              AF((AF & ~0x3b) | cbitsDup16Table(sum >> 8))
              IX(sum)

            case (0x2a) => // LD IX,(nnnn)
              addTStates(15)
              val tmp: Int = MMU.get16(PC).intValue
              CHECK_BREAK_WORD(tmp)
              IX(MMU.get16(UInt(tmp)))
              PC(PC + 2)

            case (0x2b) => // DEC IX
              addTStates(9)
              IX.decrement()
            case (0x2c) => // INC IXL
              addTStates(9)
              IXL.increment()
              AF((AF & ~0xfe) | incZ80Table(IXL.intValue))

            case (0x2d) => // DEC IXL
              addTStates(9)
              IXL.decrement()
              AF((AF & ~0xfe) | decZ80Table(IXL.intValue & 0xff))

            case (0x2e) => // LD IXL,nn
              addTStates(9)
              IXL(MMU.get8(PC))
              PC.increment()

            case (0x34) => // INC (IX+dd)
              addTStates(23)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              val temp: UByte = UByte((MMU.get8(UInt(adr)) + UByte(1)).toByte)
              MMU.put8(adr, temp)
              AF((AF & ~0xfe) | incZ80Table(temp.intValue))


            case (0x35) => // DEC (IX+dd)
              addTStates(23)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              val temp: UByte = UByte((MMU.get8(UInt(adr)) - UByte(1)).toByte)
              MMU.put8(adr, temp)
              AF((AF & ~0xfe) | decZ80Table(temp.intValue & 0xff))
            case (0x36) => // LD (IX+dd),nn
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, MMU.get8(PC))
              PC.increment()
            case (0x39) => // ADD IX,SP
              addTStates(15)
              val sum: Int = IX.intValue + SP.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IX ^ SP ^ sum) >> 8))
              IX(sum)

            case (0x44) => // LD B,IXH
              addTStates(9)
              B(IXH)

            case (0x45) => // LD B,IXL
              addTStates(9)
              B(IXL)
            case (0x46) => // LD B,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              B(MMU.get8(UInt(adr)))
            case (0x4c) => // LD C,IXH
              addTStates(9)
              C(IXH)

            case (0x4d) => // LD C,IXL
              addTStates(9)
              C(IXL)
            case (0x4e) => // LD C,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              C(MMU.get8(UInt(adr)))
            case (0x54) => // LD D,IXH
              addTStates(9)
              D(IXH)
            case (0x55) => // LD D,IXL
              addTStates(9)
              D(IXL)
            case (0x56) => // LD D,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              D(MMU.get8(UInt(adr)))
            case (0x5c) => // LD E,IXH
              addTStates(9)
              E(IXH)
            case (0x5d) => // LD E,IXL
              addTStates(9)
              E(IXL)
            case (0x5e) => // LD E,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              E(MMU.get8(UInt(adr)))
            case (0x60) => // LD IXH,B
              addTStates(9)
              IXH(B)
            case (0x61) => // LD IXH,C
              addTStates(9)
              IXH(C)

            case (0x62) => // LD IXH,D
              addTStates(9)
              IXH(D)

            case (0x63) => // LD IXH,E
              addTStates(9)
              IXH(E)

            case (0x64) => // LD IXH,IXH
              addTStates(9)
            case (0x65) => // LD IXH,IXL
              addTStates(9)
              IXH(IXL)
            case (0x66) => // LD H,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              H(MMU.get8(UInt(adr)))
            case (0x67) => // LD IXH,A
              addTStates(9)
              IXH(A)
            case (0x68) => // LD IXL,B
              addTStates(9)
              IXL(B)
            case (0x69) => // LD IXL,C
              addTStates(9)
              IXL(C)

            case (0x6a) => // LD IXL,D
              addTStates(9)
              IXL(D)

            case (0x6b) => // LD IXL,E
              addTStates(9)
              IXL(E)

            case (0x6c) => // LD IXL,IXH
              addTStates(9)
              IXL(IXH)

            case (0x6d) => // LD IXL,IXL
              addTStates(9)

            case (0x6e) => // LD L,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              L(MMU.get8(UInt(adr)))
            case (0x6f) => // LD IXL,A
              addTStates(9)
              IXL(A)

            case (0x70) => // LD (IX+dd),B
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, B)
            case (0x71) => // LD (IX+dd),C
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, C)

            case (0x72) => // LD (IX+dd),D
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, D)

            case (0x73) => // LD (IX+dd),E
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, E)

            case (0x74) => // LD (IX+dd),H
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, H)

            case (0x75) => // LD (IX+dd),L
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, L)

            case (0x77) => // LD (IX+dd),A
              addTStates(19)
              val adr = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              MMU.put8(adr, A)

            case (0x7c) => // LD A,IXH
              addTStates(9)
              A(IXH)

            case (0x7d) => // LD A,IXL
              addTStates(9)
              A(IXL)
            case (0x7e) => // LD A,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC)
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              A(MMU.get8(UInt(adr)))
            case (0x84) => // ADD A,IXH
              addTStates(9)
              val tmp: Int = IXH.intValue
              val acu: Int = A.intValue
              val sum: Int = acu + tmp
              AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)
            case (0x85) => // ADD A,IXL
              addTStates(9)
              val tmp: Int = IXL.intValue
              val acu: Int = A.intValue
              val sum: Int = acu + tmp
              AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)
            case (0x86) => // ADD A,(IX+dd)
              addTStates(19)
              val adr: Int = IX.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              val temp: Int = MMU.get8(UInt(adr)).intValue
              val acu: Int = A.intValue
              val sum: Int = acu + temp
              AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

            case (0x8c) => // ADC A,IXH
              addTStates(9)
              val temp: Int = IXH.intValue
              val acu: Int = A.intValue
              val sum: Int = acu + temp + (if (testFlag(F, FLAG_C)) 1 else 0)
              AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

            case (0x8d) => // ADC A,IXL
              addTStates(9)
              val temp:Int = IXL.intValue
              val acu:Int = A.intValue
              val sum:Int = acu + temp + (if(testFlag(F,FLAG_C)) 1 else 0)
              AF ( (addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

            case (0x8e) => // ADC A,(IX+dd)
              addTStates(19)
              val adr:Int = IX.intValue + MMU.get8(PC).intValue
              CHECK_BREAK_BYTE(adr)
              val temp:Int = MMU.get8(UInt(adr)).intValue
              val acu:Int = A.intValue
              val sum:Int = acu + temp + (if(testFlag(F,FLAG_C)) 1 else 0)
              AF ( (addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

            case (0x96) => // SUB (IX+dd)
              addTStates(19)
              val adr:Int = IX.intValue + MMU.get8(PC).intValue
              CHECK_BREAK_BYTE(adr)
              val temp:Int = MMU.get8(UInt(adr)).intValue
              val acu:Int = A.intValue
              val sum:Int = acu - temp
              AF ( (addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)
            case (0x94) => // SUB IXH
              addTStates(9)
              val temp:Int = IXH.intValue
              val acu:Int = A.intValue
              val sum:Int = acu - temp
              AF ( (addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

            case (0x9c) => // SBC A,IXH
              addTStates(9)
              val temp:Int = IXH.intValue
              val acu:Int = A.intValue
              val sum:Int = acu - temp - (if(testFlag(F,FLAG_C)) 1 else 0)
              AF ( (addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

            case (0x95) => // SUB IXL
              addTStates(9)
              val temp:Int = IXL.intValue
              val acu:Int = A.intValue
              val sum:Int = acu - temp
              AF ( (addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

            case (0x9d) => // SBC A,IXL
              addTStates(9)
              val temp:Int = IXL.intValue
              val acu:Int = A.intValue
              val sum:Int = acu - temp - (if(testFlag(F,FLAG_C)) 1 else 0)
              AF ( (addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

            case (0x9e) => // SBC A,(IX+dd)
            case (0xa4) => // AND IXH
            case (0xa5) => // AND IXL
            case (0xa6) => // AND (IX+dd)
            case (0xac) => // XOR IXH
            case (0xad) => // XOR IXL
            case (0xae) => // XOR (IX+DD)
            case (0xb4) => // OR IXH
            case (0xb5) => // OR IXL
            case (0xb6) => // OR (IX+dd)
            case (0xbc) => // CP IXH
            case (0xbd) => // CP IXL
            case (0xbe) => // CP (IX+dd)
            case (0xcb) => // CB PREFIX
            case (0xe1) => // POP IX
            case (0xe3) => // EX (SP),IX
            case (0xe5) => // PUSH IX
            case (0xe9) => // JP (IX)
            case (0xf9) => // LD SP,IX
            case _ =>
              PC.decrement()
          }
        case (0xde) => // SBC A,nn
        case (0xdf) => // RST 18H
        case (0xe0) => // RET PO
        case (0xe1) => // POP HL
        case (0xe2) => // JP PO,nnnn
        case (0xe3) => //  EX (SP),HL
        case (0xe4) => // CALL PO,nnnn
        case (0xe5) => // PUSH HL
        case (0xe6) => // AND nn
        case (0xe7) => // RST 20H
        case (0xe8) => // RET PE
        case (0xe9) => // JP (HL)
        case (0xea) => // JP PE,nnnn
        case (0xeb) => // EX DE,HL
        case (0xec) => // CALL PE,nnnn
        case (0xed) => // ED PREFIX
          INCR(1)
          val op = MMU.get8(PC).intValue
          PC.increment()
          op match {
            case (0x40) => // IN B,(C)
            case (0x41) => // OUT (C),B
            case (0x42) => // SBC HL, BC
            case (0x43) => // LD (nnnn),BC
            case (0x44) => // NEG
            case (0x4C) => // NEG
            case (0x54) => // NEG
            case (0x64) => // NEG
            case (0x6C) => // NEG
            case (0x74) => // NEG
            case (0x7C) => // NEG
            case (0x45) => // RETN
            case (0x55) => // RETN
            case (0x5D) => // RETN
            case (0x65) => // RETN
            case (0x6D) => // RETN
            case (0x75) => // RETN
            case (0x7D) => // RETN
            case (0x46) => // IM 0
            case (0x47) => // LD I,A
            case (0x48) => // IN C,(C)
            case (0x49) => // OUT (C),C
            case (0x4a) => // ADC HL,BC
            case (0x4b) => // LD BC,(nnnn)
            case (0x4d) => // RETI
            case (0x4f) => // LD R,A
            case (0x50) => // IN D,(C)
            case (0x51) => // OUT (C),D
            case (0x52) => // SBC HL,DE
            case (0x53) => // LD (nnnn),DE
            case (0x56) => // IM 1
            case (0x57) => // LD A,I
            case (0x58) => // IN E,(C)
            case (0x59) => // OUT (C),E
            case (0x5a) => // ADC HL,DE
            case (0x5b) => // LD DE,(nnnn)
            case (0x5e) => // IM 2
            case (0x5f) => // LD A,R
            case (0x60) => // IN H,(C)
            case (0x61) => // OUT (C),H
            case (0x62) => // SBC HL,HL
            case (0x63) => // LD (nnnn),HL
            case (0x67) => // RRD
            case (0x68) => // IN L,(C)
            case (0x69) => // OUT (C),L
            case (0x6a) => // ADC HL,HL
            case (0x6b) => // LD HL,(nnnn)
            case (0x6f) => // RLD
            case (0x70) => // IN (C)
            case (0x71) => // OUT (C),0
            case (0x72) => // SBC HL,SP
            case (0x73) => // LD (nnnn),SP
            case (0x78) => // IN A,(C)
            case (0x79) => // OUT (C),A
            case (0x7a) => // ADC HL,SP
            case (0x7b) => // LD SP,(nnnn)
            case (0xa0) => // LDI
            case (0xa1) => // CPI
            case (0xa2) => // INI
            case (0xa3) => // OUTI
            case (0xa8) => // LDD
            case (0xa9) => // CPD
            case (0xaa) => // IND
            case (0xab) => // OUTD
            case (0xb0) => // LDIR
            case (0xb1) => // CPIR
            case (0xb2) => // INIR
            case (0xb3) => // OTIR
            case (0xb8) => // LDDR
            case (0xb9) => // CPDR
            case (0xba) => // INDR
            case (0xbb) => // OTDR

            case _ =>
          }

        case (0xee) => // XOR nn
        case (0xef) => // RST 28H
        case (0xf0) => // RET P
        case (0xf1) => // POP AF
        case (0xf2) => // JP P,nnnn
        case (0xf3) => // DI
        case (0xf4) => // CALL P,nnnn
        case (0xf5) => // PUSH AF
        case (0xf6) => // OR nn
        case (0xf7) => // RST 30H
        case (0xf8) => // RET M
        case (0xf9) => // LD SP,HL
        case (0xfa) => // JP M,nnnn
        case (0xfb) => // EI
        case (0xfc) => // CALL M, nnnn


        case (0xfd) => // FD Prefix
          INCR(1)
          val op: Int = MMU.get8(PC).intValue
          PC.increment()
          op match {
            case (0x09) => // ADD IY,BC
              addTStates(15)
              val sum: Int = IY.intValue + BC.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IY ^ BC ^ sum) >> 8))
              IY(sum)

            case (0x19) => // ADD IY,DE
              addTStates(15)
              val sum: Int = IY.intValue + DE.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IY ^ DE ^ sum) >> 8))
              IY(sum)


            case (0x21) => // LD IY,nnnn
              addTStates(14)
              IY(MMU.get16(PC))
              PC(PC + 2)
            case (0x22) => // LD (nnnn),IY
              addTStates(20)
              val temp: Int = MMU.get16(PC).intValue
              CHECK_BREAK_WORD(temp)
              MMU.put16(temp, IY)
              PC(PC + 2)
            case (0x23) => // INC IY
              addTStates(10)
              IY.increment()
            case (0x24) => // INC IYH
              addTStates(9)
              IYH.increment()
              AF((AF & ~0xfe) | incZ80Table(IYH))
            case (0x25) => // DEC IYH
              addTStates(9)
              IYH.decrement()
              AF((AF & ~0xfe) | decZ80Table(IYH))

            case (0x26) => // LD IYH,nn
            case (0x29) => // ADD IY,IY
            case (0x2a) => // LD IY,(nnnn)
            case (0x2b) => // DEC IY
            case (0x2c) => // INC IYL
            case (0x2d) => // DEC IYL
            case (0x2e) => // LD IYL,nn
            case (0x34) => // INC (IY+dd)
            case (0x35) => // DEC (IY+dd)
            case (0x36) => // LD (IY+dd),nn
            case (0x39) => // ADD IY,SP
              addTStates(15)
              val sum: Int = IY.intValue + SP.intValue
              AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IY ^ SP ^ sum) >> 8))
              IY(sum)


            case (0x44) => // LD B,IYH
              addTStates(9)
              B(IYH)
            case (0x45) => // LD B,IYL
              addTStates(9)
              B(IYL)
            case (0x46) => // LD B,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              B(MMU.get8(UInt(adr)))
            case (0x4c) => // LD C,IYH
              addTStates(9)
              C(IYH)
            case (0x4d) => // LD C,IYL
              addTStates(9)
              C(IYL)
            case (0x4e) => // LD C,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              C(MMU.get8(UInt(adr)))
            case (0x54) => // LD D,IYH
              addTStates(9)
              D(IYH)
            case (0x55) => // LD D,IYL
              addTStates(9)
              D(IYL)
            case (0x56) => // LD D,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              D(MMU.get8(UInt(adr)))
            case (0x5c) => // LD E,IYH
              addTStates(9)
              E(IYH)
            case (0x5d) => // LD E,IYL
              addTStates(9)
              E(IYL)
            case (0x5e) => // LD E,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              E(MMU.get8(UInt(adr)))
            case (0x60) => // LD IYH,B
              addTStates(9)
              IYH(B)
            case (0x61) => // LD IYH,C
              addTStates(9)
              IYH(C)
            case (0x62) => // LD IYH,D
              addTStates(9)
              IYH(D)
            case (0x63) => // LD IYH,E
              addTStates(9)
              IYH(E)
            case (0x64) => // LD IYH,IYH
              addTStates(9)
            case (0x65) => // LD IYH,IYL
              addTStates(9)
              IYH(IYL)
            case (0x66) => // LD H,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              H(MMU.get8(UInt(adr)))
            case (0x67) => // LD IYH,A
              addTStates(9)
              IYH(A)
            case (0x68) => // LD IYL,B
              addTStates(9)
              IYL(B)
            case (0x69) => // LD IYL,C
            case (0x6a) => // LD IYL,D
            case (0x6b) => // LD IYL,E
            case (0x6c) => // LD IYL,IYH
            case (0x6e) => // LD L,(IY+dd)
              addTStates(19)
              val adr: Int = IY.intValue + MMU.get8(PC).intValue
              PC.increment()
              CHECK_BREAK_BYTE(adr)
              L(MMU.get8(UInt(adr)))
            case (0x6f) => // LD IYL,A
            case (0x70) => // LD (IY+dd),B
            case (0x71) => // LD (IY+dd),C
            case (0x72) => // LD (IY+dd),D
            case (0x73) => // LD (IY+dd),E
            case (0x74) => // LD (IY+dd),H
            case (0x75) => // LD (IY+dd),L
            case (0x77) => // LD (IY+dd),A
            case (0x7c) => // LD A,IYH
            case (0x7e) => // LD A,(IY+dd)
            case (0x84) => // ADD A,IYH
            case (0x85) => // ADD A,IYL
            case (0x86) => // ADD A,(IY+dd)
            case (0x8c) => // ADC A,IYH
            case (0x8d) => // ADC,IYL
            case (0x8e) => // ADC A,(IY+dd)
            case (0x96) => // SUB (IY+dd)
            case (0x94) => // SUB IYH
            case (0x9c) => // SBC A,IYH
            case (0x95) => // SUB IYL
            case (0x9d) => // SBC A,IYL
            case (0x9e) => // SBC A,(IYL+dd)
            case (0xa4) => // AND IYH
            case (0xa5) => // AND IYL
            case (0xa6) => // AND (IY+dd)
            case (0xac) => // XOR IYH
            case (0xad) => // XOR IYL
            case (0xae) => // XOR (IY+dd)
            case (0xb4) => // OR IYH
            case (0xb5) => // OR IYL
            case (0xbc) => // CP IYH
            case (0xbd) => // CP IYL
            case (0xbe) => // CP (IY+dd)
            case (0xcb) => // CB Prefix
            case (0xe1) => // POP IY
            case (0xe3) => // EX (SP),IY
            case (0xe5) => // PUSH IY
            case (0xe9) => // JP (IY)
            case (0xf9) => // LD SP,IY


            case _ =>
              PC.decrement()
          }

        case (0xfe) => // CP nn
        case (0xff) => // RST 38H

      }


      SimTimer.sim_interval = SimTimer.sim_interval - 1


    } // end SwitchCPUNow

    // TODO simulation halted
    onHalt()
  }

  override def onHalt(): Unit = {
    Utils.outln(s"$name: Halted.")
    Utils.outln(showRegisters())
    Utils.outln(showFlags())
  }

  @inline
  private def addTStates(x: Long): Unit = tStates = tStates + x

  @inline
  private def INCR(count: Int): Unit = {
    R.set8(UByte(((R.get8 & ~0x7f) | ((R.get8 + count) & 0x7f)).toByte)) // Increment R
  }

  @inline
  // TODO Implement memory break points.
  private def CHECK_BREAK_BYTE(reg: Register16): Boolean = {
    false
  }

  @inline
  private def CHECK_BREAK_BYTE(v: UShort): Boolean = false

  @inline // TODO Implement memory break points.
  private def CHECK_BREAK_WORD(reg: Register16): Boolean = false

  @inline
  def CHECK_BREAK_BYTE(v: Int): Boolean = false


  @inline
  def CHECK_BREAK_WORD(addr: Int): Boolean = false

  private val incTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0) 1 else 0) << 4
      UByte(((temp & 0xa8) | (t1 | t2)).toByte)
    }
  }.toArray

  private val incZ80Table: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0) 1 else 0) << 4
      val t3 = (if (temp == 0x80) 1 else 0) << 2
      UByte(((temp & 0xa8) | (t1 | (t2 | t3))).toByte)
    }
  }.toArray


  private val parityTable: Array[UByte] = {
    for (i <- 0 to 255) yield {
      if ((((i & 1) + ((i & 2) >> 1) + ((i & 4) >> 2) + ((i & 8) >> 3) +
        ((i & 16) >> 4) + ((i & 32) >> 5) + ((i & 64) >> 6) + ((i & 128) >> 7)) % 2) != 0)
        UByte(0) else UByte(4)
    }
  }.toArray

  private val decTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0xf) 1 else 0) << 4
      UByte((((temp & 0xa8) | (t1 | t2)) | 2).toByte)
    }
  }.toArray

  private val decZ80Table: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0xf) 1 else 0) << 4
      val t3 = (if (temp == 0x7f) 1 else 0) << 2
      UByte(((temp & 0xa8) | (t1 | (t2 | t3)) | 2).toByte)
    }
  }.toArray

  private val cbitsTable: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | ((cbits >> 8) & 1)).toByte)
    }
  }.toArray

  private val cbitsZ80Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | (((cbits >> 6) ^ (cbits >> 5)) & 4) |
        ((cbits >> 8) & 1)).toByte)
    }
  }.toArray

  private val cbitsDup8Table: Array[UShort] = {
    for (cbits <- 0 to 511) yield {
      val t1 = if ((cbits & 0xff) == 0) 1 else 0
      UShort(((cbits & 0x10) | ((cbits >> 8) & 1) | ((cbits & 0xff) << 8) | (cbits & 0xa8) | t1 << 6).toShort)
    }
  }.toArray

  private val cbitsZ80DupTable: Array[UShort] = {
    for (cbits <- 0 to 511) yield {
      val t1 = if ((cbits & 0xff) == 0) 1 else 0
      UShort(((cbits & 0x10) | (((cbits >> 6) ^ (cbits >> 5)) & 4) |
        ((cbits >> 8) & 1) | (cbits & 0xa8)).toShort)
    }
  }.toArray

  private val cbitsDup16Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | ((cbits >> 8) & 1) | (cbits & 0x28)).toByte)
    }
  }.toArray

  private val cbits2Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | ((cbits >> 8) & 1) | 2).toByte)
    }
  }.toArray

  private val cbits2Z80Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((((cbits >> 6) ^ (cbits >> 5)) & 4) | (cbits & 0x10) | 2 | ((cbits >> 8) & 1)).toByte)
    }
  }.toArray

  private val cbits2Z80DupTable: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((((cbits >> 6) ^ (cbits >> 5)) & 4) | (cbits & 0x10) | 2 | ((cbits >> 8) & 1) |
        (cbits & 0xa8)).toByte)
    }
  }.toArray

  private val rrcaTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort((((temp & 1) << 15) | (sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private val rraTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort(((sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private val addTable: Array[UShort] = {
    for (sum <- 0 to 511) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UShort((((sum & 0xff) << 8) | (sum & 0xa8) | (t1 << 6)).toShort)
    }
  }.toArray

  private val subTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UShort((((sum & 0xff) << 8) | (sum & 0xa8) | (t1 << 6) | 2).toShort)
    }
  }.toArray

  private val andTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if (sum == 0) 1 else 0
      UShort(((sum << 8) | (sum & 0xa8) | (t1 << 6) | 0x10 | parityTable(sum)).toShort)
    }
  }.toArray

  private val xororTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if (sum == 0) 1 else 0
      UShort(((sum << 8) | (sum & 0xa8) | (t1 << 6) | parityTable(sum)).toShort)
    }
  }.toArray

  @inline
  private def PARITY(value: Int): UByte = parityTable(value & 0xff)

  private val rotateShiftTable: Array[UByte] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UByte(((sum & 0xa8) | (t1 << 6) | PARITY(sum)).toByte)
    }
  }.toArray

  private val negTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = if ((temp & 0x0f) != 0) 1 else 0
      val t2 = if (temp == 0x80) 1 else 0
      UByte(((t1 << 4) | (t2 << 2) | 2 | {
        if (temp != 0) 1 else 0
      }).toByte)
    }
  }.toArray

  private val rrdrldTable: Array[UByte] = {
    for (acu <- 0 to 255) yield {
      val t1 = if ((acu & 0xff) == 0) 1 else 0
      UByte(((acu << 8) | (acu & 0xa8) | (t1 << 6) | parityTable(acu)).toByte)
    }
  }.toArray

  private def cpTable: Array[UByte] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UByte(((sum & 0x80) | (t1 << 6)).toByte)
    }
  }.toArray

  @inline
  private def SET_PV2(x: UByte, temp: Register8): UByte = {
    UByte(({
      if (temp.get8 == x) 1 else 0
    } << 2).toByte)
  }

  @inline
  private def SET_PV2(x: Int, temp: Register8): UByte = SET_PV2(UByte(x.toByte), temp)

  @inline
  private def SET_PV2(x: Int, temp: Int): UByte = {
    UByte(({
      if (temp == x) 1 else 0
    } << 2).toByte)
  }

  @inline
  private def SET_PV(cbits: Int): Int = (cbits >> 6) ^ (cbits >> 5) & 4

  @inline
  def POP(x: Register16): Unit = {
    val y = MMU.get8(SP)
    SP.increment()
    x(y + (MMU.get8(SP) << 8).shortValue)
    SP.increment()
  }

  @inline
  def PUSH(x: Register16): Unit = {
    PUSH(x.get16.intValue)
  }

  @inline
  def PUSH(x: Int): Unit = {
    SP.decrement()
    MMU.put8(SP, UByte((x >> 8).byteValue))
    SP.decrement()
    MMU.put8(SP, UByte((x & 0xff).byteValue))
  }


  @inline
  def JPC(cond: => Boolean): Unit = {
    addTStates(10)
    if (cond) {
      PC(MMU.get16(PC))
    } else PC(PC + 2)
  }

  @inline
  def CALLC(cond: => Boolean): Unit = {
    if (cond) {
      val addr = MMU.get16(PC)
      CHECK_BREAK_WORD(SP - 2)
      PUSH(PC + 2)
      PC(addr)
      addTStates(17)
    } else {
      PC(PC + 2)
      addTStates(10)
    }
  }
}
