package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}
import com.sim.{SimTimer, Utils}

import scala.annotation.switch

class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "Z80"
  override val MMU: BasicMMU = new Z80MMU(this)
  override val description: String = "Z80 CPU"

  override def init(): Unit = {} // TODO

  var tStates: Long = 0L

  override def createUnitOptions: Unit = {
    // Set up CPU common options.
    super.createUnitOptions

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("ALTAIRROM", "Enable Altair ROM.", value = false))
    unitOptions.append(ValueUnitOption("MEMORY", "Set the RAM size.", value = 0xFFFF))

  }

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
    I(0x00) // Interrupts disabled
    IFF(0x00)
    HLP(0x0000)
    BCP(0x0000)
    AFP(0x0000)
    DEP(0x0000)
    AFP(0x0000)

  }

  override def showCommand(stringBuilder: StringBuilder): Unit = {
    super.showCommand(stringBuilder)
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
  val R = new Register8("R")
  val IR = new CompositeRegister16("IR", I, R) // Interrupt/Refresh
  val IFF = new Register8("IFF") // Interrupt ena/disable
  val HP = new Register8("H'")
  val LP = new Register8("L'")
  val AP = new Register8("A'")
  val FP = new Register8("F'")
  val BP = new Register8("B'")
  val CP = new Register8("C'")
  val DP = new Register8("D'")
  val EP = new Register8("E'")
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
    "AP" -> AP, "FP" -> FP, "AFP" -> AFP, "IFF" -> IFF, "IR" -> IR
  )

  resetCPU()

  override def showRegisters(): String = {
    s"$PC  $SP  $AF  $BC  $DE  $HL  $IX  $IY  $R  $I\n                     $AFP $BCP $DEP $HLP\n$IFF"
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

    try {
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
        val instr: Int = MMU.get8(PC)
        PC.increment()

        (instr: @switch) match {

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
            INC(B)

          case (0x05) => // DEC B
            addTStates(4)
            DEC(B)

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
            ADD(HL, BC)

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
            INC(C)

          case (0x0d) => // DEC C
            addTStates(4)
            DEC(C)

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
            INC(D)

          case (0x15) => // DEC D
            addTStates(4)
            DEC(D)

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
            ADD(HL, DE)

          case (0x1a) => // LD A, (DE)
            addTStates(7)
            memoryBreak = CHECK_BREAK_BYTE(DE)
            if (!memoryBreak) A(MMU.get8(DE))

          case (0x1b) => // DEC DE
            addTStates(6)
            DE.decrement()

          case (0x1c) => // INC E
            addTStates(4)
            INC(E)

          case (0x1d) => // DEC E
            addTStates(4)
            DEC(E)

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
            INC(H)

          case (0x25) => // DEC H
            addTStates(4)
            DEC(H)

          case (0x26) => // LD H,nn
            addTStates(7)
            H(MMU.get8(PC))
            PC.increment()

          case (0x27) => // DAA
            addTStates(4)
            var tmp1 = A.get8.intValue
            var tmp2 = 0
            var tmp3 = F & 1
            var tmp = tmp3
            if (((F & 0x10) != 0) || ((tmp1 & 0x0f) > 0x09)) tmp2 |= 0x06
            if ((tmp3 == 1) || (tmp1 > 0x9f) || ((tmp1 > 0x8f) && ((tmp1 & 0x0f) > 0x09))) {
              tmp2 |= 0x60
              tmp = 1
            }
            if (tmp1 > 0x99) tmp = 1
            if ((F & 0x02) != 0) {
              //cycle -= 4
              SUB(UInt(tmp2))
            }
            else {
              //cycle -= 4
              ADD(A, UInt(tmp2))
            }
            F((F & 0xfe) | tmp)
            if (parity(A)) F((F & 0xfb) | 4)
            F(F & 0xfb)

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
            ADD(HL, HL)

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
            INC(L)

          case (0x2d) => // DEC L
            addTStates(4)
            DEC(L)

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
            MMU.put8(MMU.get16(PC), A)
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
            ADD(HL, SP)

          case (0x3a) => // LD A,(nnnn)
            addTStates(13)
            val tmp: Int = MMU.get16(PC)
            CHECK_BREAK_BYTE(tmp)
            A(MMU.get8(tmp))
            PC(PC + 2)

          case (0x3b) => // DEC SP
            addTStates(6)
            SP.decrement()

          case (0x3c) => // INC A
            addTStates(4)
            INC(A)

          case (0x3d) => // DEC A
            addTStates(4)
            DEC(A)

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
            ADD(A, B)

          case (0x81) => // ADD A,C
            addTStates(4)
            ADD(A, C)

          case (0x82) => // ADD A,D
            addTStates(4)
            ADD(A, D)

          case (0x83) => // ADD A,E
            addTStates(4)
            ADD(A, E)

          case (0x84) => // ADD A,H
            addTStates(4)
            ADD(A, H)

          case (0x85) => // ADD A,L
            addTStates(4)
            ADD(A, L)

          case (0x86) => // ADD A,(HL)
            addTStates(7)
            CHECK_BREAK_BYTE(HL)
            val temp: UInt = MMU.get8(HL)
            val acu: UInt = A.get8
            val sum: UInt = acu + temp
            val cbits: UInt = acu ^ temp ^ sum
            AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

          case (0x87) => // ADD A,A
            addTStates(4)
            val cbits: UInt = UInt(2) * A.get8
            AF(cbitsDup8Table(cbits & 0x1ff) | SET_PV(cbits))


          case (0x88) => // ADC A,B
            addTStates(4)
            ADC(A, B)

          case (0x89) => // ADC A,C
            addTStates(4)
            ADC(A, C)

          case (0x8a) => // ADC A,D
            addTStates(4)
            ADC(A, E)

          case (0x8b) => // ADC A,E
            addTStates(4)
            ADC(A, E)

          case (0x8c) => // ADC A,H
            addTStates(4)
            ADC(A, H)

          case (0x8d) => // ADC A,L
            addTStates(4)
            ADC(A, L)

          case (0x8e) => // ADC A,(HL)
            addTStates(7)
            CHECK_BREAK_BYTE(HL)
            val temp: UInt = MMU.get8(HL)
            val acu: UInt = A.get8
            val sum: UInt = acu + temp + {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            val cbits: UInt = acu ^ temp ^ sum
            AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

          case (0x8f) => // ADC A,A
            addTStates(4)
            val cbits: UInt = UInt(2) * A.get8 + {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            AF(cbitsDup8Table(cbits & 0x1ff) | SET_PV(cbits))

          case (0x90) => // SUB B
            addTStates(4)
            SUB(B)

          case (0x91) => // SUB C
            addTStates(4)
            SUB(C)

          case (0x92) => // SUB D
            addTStates(4)
            SUB(D)

          case (0x93) => // SUB E
            addTStates(4)
            SUB(E)

          case (0x94) => // SUB H
            addTStates(4)
            SUB(H)

          case (0x95) => // SUB L
            addTStates(4)
            SUB(L)

          case (0x96) => // SUB (HL)
            addTStates(7)
            CHECK_BREAK_BYTE(HL)
            val temp: UInt = MMU.get8(HL)
            val acu: UInt = A.get8
            val sum: UInt = acu - temp
            val cbits: UInt = acu ^ temp ^ sum
            AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

          case (0x97) => // SUB A
            addTStates(4)
            AF(0x42)

          case (0x98) => // SBC A,B
            addTStates(4)
            SBC(A, B)

          case (0x99) => // SBC A,C
            addTStates(4)
            SBC(A, C)

          case (0x9a) => // SBC A,D
            addTStates(4)
            SBC(A, D)

          case (0x9b) => // SBC A,E
            addTStates(4)
            SBC(A, E)

          case (0x9c) => // SBC A,H
            addTStates(4)
            SBC(A, H)

          case (0x9d) => // SBC A,L
            addTStates(4)
            SBC(A, L)

          case (0x9e) => // SBC A,(HL)
            addTStates(7)
            CHECK_BREAK_BYTE(HL)
            val temp: UInt = MMU.get8(HL)
            val acu: UInt = A.get8
            val sum: UInt = acu - temp - {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            val cbits: UInt = acu ^ temp ^ sum
            AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

          case (0x9f) => // SBC A,A
            addTStates(4)
            val cbits: UInt = {
              if (testFlag(F, FLAG_C)) UInt(-1) else UInt(0)
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
            AF(andTable(A & MMU.get8(HL)))

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
            AF(xororTable(A ^ MMU.get8(HL)))

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
            AF(xororTable(A | MMU.get8(HL)))

          case (0xb7) => // OR A
            addTStates(4)
            AF(xororTable(A))

          case (0xb8) => // CP B
            addTStates(4)
            ICP(B)

          case (0xb9) => // CP C
            addTStates(4)
            ICP(C)

          case (0xba) => // CP D
            addTStates(4)
            ICP(D)

          case (0xbb) => // CP E
            addTStates(4)
            ICP(E)

          case (0xbc) => // CP H
            addTStates(4)
            ICP(H)

          case (0xbd) => // CP L
            addTStates(4)
            ICP(L)

          case (0xbe) => // CP (HL)
            addTStates(7)
            CHECK_BREAK_BYTE(HL)
            val temp: UInt = MMU.get8(HL)
            val acu: UInt = A.get8
            AF(AF & ~0x28 | temp & 0x28)
            val sum: UInt = acu - temp
            val cbits: UInt = acu ^ temp ^ sum
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
            val temp: UInt = MMU.get8(PC)
            PC.increment()
            val acu: UInt = A.get8
            val sum: UInt = acu + temp
            val cbits: UInt = acu ^ temp ^ sum
            AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

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

          // ******************************************************************************** CB
          case (0xcb) => // CB prefix
            INCR(1)
            val adr: Int = HL.get16
            val op: Int = MMU.get8(PC)
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
                acu = MMU.get8(adr)
                addTStates(15)
                tStateModifier = true
              case (7) =>
                PC.increment()
                acu = A
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
            val temp: UInt = MMU.get8(PC)
            PC.increment()
            val acu: UInt = A.get8
            val sum: UInt = acu + temp + {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            val cbits: UInt = acu ^ temp ^ sum
            AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

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
            MMU.out8(MMU.get8(PC), A)

          case (0xd4) => // CALL NC, nnnn
            CALLC(!testFlag(F, FLAG_C))

          case (0xd5) => // PUSH DE
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(DE)

          case (0xd6) => // SUB nn
            addTStates(7)
            val temp: UInt = MMU.get8(PC)
            PC.increment()
            val acu: UInt = A.get8
            val sum: UInt = acu - temp
            val cbits: UInt = acu ^ temp ^ sum
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
            A(MMU.in8(MMU.get8(PC)))
            PC.increment()

          case (0xdc) => // CALL C,nnnn
            CALLC(testFlag(F, FLAG_C))


          // ************************************************************************************** DD
          case (0xdd) => // DD Prefix
            INCR(1)
            val op: Int = MMU.get8(PC)
            PC.increment()
            op match {
              case (0x09) => // ADD IX,BC
                addTStates(15)
                ADD(IX, BC)

              case (0x19) => // ADD IX,DE
                addTStates(15)
                ADD(IX, DE)

              case (0x21) => // LD IX,nnnn
                addTStates(14)
                IX(MMU.get16(PC))
                PC(PC + 2)

              case (0x22) => // LD (nnnn),IX
                addTStates(20)
                val temp: Int = MMU.get16(PC)
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
                ADD(IX, IX)

              case (0x2a) => // LD IX,(nnnn)
                addTStates(15)
                val tmp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(tmp)
                IX(MMU.get16(tmp))
                PC(PC + 2)

              case (0x2b) => // DEC IX
                addTStates(9)
                IX.decrement()

              case (0x2c) => // INC IXL
                addTStates(9)
                IXL.increment()
                AF((AF & ~0xfe) | incZ80Table(IXL))

              case (0x2d) => // DEC IXL
                addTStates(9)
                IXL.decrement()
                AF((AF & ~0xfe) | decZ80Table(IXL & 0xff))

              case (0x2e) => // LD IXL,nn
                addTStates(9)
                IXL(MMU.get8(PC))
                PC.increment()

              case (0x34) => // INC (IX+dd)
                addTStates(23)
                INCIDXdd(IX)

              case (0x35) => // DEC (IX+dd)
                addTStates(23)
                DECIDXdd(IX)

              case (0x36) => // LD (IX+dd),nn
                addTStates(19)
                val adr: Int = IX + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                MMU.put8(adr, MMU.get8(PC))
                PC.increment()

              case (0x39) => // ADD IX,SP
                addTStates(11)
                ADD(IX, SP)

              case (0x44) => // LD B,IXH
                addTStates(9)
                B(IXH)

              case (0x45) => // LD B,IXL
                addTStates(9)
                B(IXL)

              case (0x46) => // LD B,(IX+dd)
                addTStates(19)
                LDIDXdd(B, IX)

              case (0x4c) => // LD C,IXH
                addTStates(9)
                C(IXH)

              case (0x4d) => // LD C,IXL
                addTStates(9)
                C(IXL)

              case (0x4e) => // LD C,(IX+dd)
                addTStates(19)
                LDIDXdd(C, IX)

              case (0x54) => // LD D,IXH
                addTStates(9)
                D(IXH)

              case (0x55) => // LD D,IXL
                addTStates(9)
                D(IXL)

              case (0x56) => // LD D,(IX+dd)
                addTStates(19)
                LDIDXdd(D, IX)

              case (0x5c) => // LD E,IXH
                addTStates(9)
                E(IXH)

              case (0x5d) => // LD E,IXL
                addTStates(9)
                E(IXL)

              case (0x5e) => // LD E,(IX+dd)
                addTStates(19)
                LDIDXdd(E, IX)

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
                LDIDXdd(H, IX)

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
                LDIDXdd(L, IX)

              case (0x6f) => // LD IXL,A
                addTStates(9)
                IXL(A)

              case (0x70) => // LD (IX+dd),B
                addTStates(19)
                LDIDXdd(IX, B)

              case (0x71) => // LD (IX+dd),C
                addTStates(19)
                LDIDXdd(IX, C)

              case (0x72) => // LD (IX+dd),D
                addTStates(19)
                LDIDXdd(IX, D)

              case (0x73) => // LD (IX+dd),E
                addTStates(19)
                LDIDXdd(IX, E)

              case (0x74) => // LD (IX+dd),H
                addTStates(19)
                LDIDXdd(IX, H)

              case (0x75) => // LD (IX+dd),L
                addTStates(19)
                LDIDXdd(IX, L)

              case (0x77) => // LD (IX+dd),A
                addTStates(19)
                LDIDXdd(IX, A)

              case (0x7c) => // LD A,IXH
                addTStates(9)
                A(IXH)

              case (0x7d) => // LD A,IXL
                addTStates(9)
                A(IXL)

              case (0x7e) => // LD A,(IX+dd)
                addTStates(19)
                LDIDXdd(A, IX)

              case (0x84) => // ADD A,IXH
                addTStates(4)
                ADDIDX(A, IXH)

              case (0x85) => // ADD A,IXL
                addTStates(9)
                ADDIDX(A, IXL)

              case (0x86) => // ADD A,(IX+dd)
                addTStates(19)
                val adr: Int = IX + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: Int = MMU.get8(adr)
                val acu: Int = A
                val sum: Int = acu + temp
                AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

              case (0x8c) => // ADC A,IXH
                addTStates(9)
                ADCIDX(A, IXH)

              case (0x8d) => // ADC A,IXL
                addTStates(9)
                ADCIDX(A, IXL)

              case (0x8e) => // ADC A,(IX+dd)
                addTStates(19)
                val adr: Int = IX + MMU.get8(PC)
                CHECK_BREAK_BYTE(adr)
                val temp: Int = MMU.get8(adr)
                val acu: Int = A
                val sum: Int = acu + temp + (if (testFlag(F, FLAG_C)) 1 else 0)
                AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

              case (0x96) => // SUB (IX+dd)
                addTStates(19)
                val adr: Int = IX + MMU.get8(PC)
                CHECK_BREAK_BYTE(adr)
                val temp: UInt = MMU.get8(adr)
                val acu: UInt = A.get8
                val sum: UInt = acu - temp
                AF((addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

              case (0x94) => // SUB IXH
                addTStates(9)
                SUBIDX(IXH)

              case (0x9c) => // SBC A,IXH
                addTStates(9)
                SBCIDX(A, IXH)

              case (0x95) => // SUB IXL
                addTStates(9)
                SUBIDX(IXL)

              case (0x9d) => // SBC A,IXL
                addTStates(9)
                SBCIDX(A, IXL)

              case (0x9e) => // SBC A,(IX+dd)
                addTStates(19)
                val adr: Int = IX + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: UInt = MMU.get8(adr)
                val acu: UInt = A.get8
                val sum: UInt = acu - temp - (if (testFlag(F, FLAG_C)) UInt(1) else UInt(0))
                AF((addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

              case (0xa4) => // AND IXH
                addTStates(9)
                AF(andTable((AF & IX) >> 8 & 0xff))

              case (0xa5) => // AND IXL
                addTStates(9)
                AF(andTable(((AF >> 8) & IX) & 0xff))

              case (0xa6) => // AND (IX+dd)
                addTStates(19)
                ANDIDXdd(IX)

              case (0xac) => // XOR IXH
                addTStates(9)
                AF(xororTable(((AF & IX) >> 8) & 0xff))

              case (0xad) => // XOR IXL
                addTStates(9)
                AF(xororTable(((AF >> 8) ^ IX) & 0xff))

              case (0xae) => // XOR (IX+DD)
                addTStates(19)
                XORIDXdd(IX)

              case (0xb4) => // OR IXH
                addTStates(9)
                AF(xororTable(((AF | IX) >> 8) & 0xff))

              case (0xb5) => // OR IXL
                addTStates(9)
                AF(xororTable(((AF >> 8) | IX) & 0xff))

              case (0xb6) => // OR (IX+dd)
                addTStates(19)
                ORIDXdd(IX)

              case (0xbc) => // CP IXH
                addTStates(9)
                CPIX8(IXH)

              case (0xbd) => // CP IXL
                addTStates(9)
                CPIX8(IXL)

              case (0xbe) => // CP (IX+dd)
                addTStates(19)
                CPIDXdd(IX)

              // ******************************************************************************** DD CB PREFIX
              case (0xcb) => {
                //  DD CB PREFIX
                val adr: Int = IX + MMU.get8(PC)
                PC.increment()
                INCR(1)
                val op: Int = MMU.get8(PC)
                var acu: Int = 0
                var temp: Int = 0
                var cbits: Int = 0

                (op & 7) match {
                  case 0 =>
                    PC.increment()
                    acu = B

                  case 1 =>
                    PC.increment()
                    acu = C

                  case 2 =>
                    PC.increment()
                    acu = D

                  case 3 =>
                    PC.increment()
                    acu = E

                  case 4 =>
                    PC.increment()
                    acu = H

                  case 5 =>
                    PC.increment()
                    acu = L

                  case 6 =>
                    CHECK_BREAK_BYTE(adr)
                    PC.increment()
                    acu = MMU.get8(adr)

                  case 7 =>
                    PC.increment()
                    acu = A

                  case _ =>

                }
                (op & 0xc0) match {
                  case (0x00) => {
                    // Shift/rotate
                    // #4438
                    addTStates(23)
                    (op & 0x38) match {
                      case (0x00) => // RLC
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
                        temp = (acu >> 1) | ({
                          if (testFlag(F, FLAG_C)) 1 else 0
                        } << 7)
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
                  }


                  case (0x40) => // BIT
                    addTStates(20)
                    if ((acu & (1 << ((op >> 3) & 7))) != 0) AF((AF & ~0xfe) | 0x10 | ({
                      if ((op & 0x38) == 0x38) 1 else 0
                    } << 7))
                    else AF((AF & ~0xfe) | 0x54)
                    if ((op & 7) != 6) AF(AF | (acu & 0x28))
                    temp = acu

                  case (0x80) => // RES
                    addTStates(23)
                    temp = acu & ~(1 << ((op >> 3) & 7))

                  case (0xc0) => // SET
                    addTStates(23)
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
              }
              case (0xe1) => // POP IX
                addTStates(14)
                CHECK_BREAK_WORD(SP)
                POP(IX)

              case (0xe3) => // EX (SP),IX
                addTStates(23)
                CHECK_BREAK_WORD(SP)
                val tmp: Int = IX
                POP(IX)
                PUSH(tmp)

              case (0xe5) => // PUSH IX
                addTStates(15)
                CHECK_BREAK_WORD(SP - 2)
                PUSH(IX)

              case (0xe9) => // JP (IX)
                addTStates(8)
                PC(IX)

              case (0xf9) => // LD SP,IX
                addTStates(10)
                SP(IX)

              case _ =>
                // Ignore DD
                PC.decrement()
            }

          case (0xde) => // SBC A,nn  #4580
            addTStates(7)
            val temp: UInt = MMU.get8(PC)
            PC.increment()
            val acu: UInt = A.get8
            val sum: UInt = acu - temp - {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            val cbits: UInt = acu ^ temp ^ sum
            AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

          case (0xdf) => // RST 18H
            addTStates(18)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(PC)
            PC(0x18)

          case (0xe0) => // RET PO
            if (testFlag(F, FLAG_P)) addTStates(5)
            else {
              CHECK_BREAK_WORD(SP)
              POP(PC)
              addTStates(11)
            }

          case (0xe1) => // POP HL
            addTStates(10)
            CHECK_BREAK_WORD(SP)
            POP(HL)

          case (0xe2) => // JP PO,nnnn
            JPC(!testFlag(F, FLAG_P))

          case (0xe3) => //  EX (SP),HL
            addTStates(19)
            CHECK_BREAK_WORD(SP)
            val temp: Int = HL
            POP(HL)
            PUSH(temp)

          case (0xe4) => // CALL PO,nnnn
            CALLC(!testFlag(F, FLAG_P))

          case (0xe5) => // PUSH HL
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(HL)

          case (0xe6) => // AND nn
            addTStates(7)
            AF(andTable(((AF >> 8) & MMU.get8(PC)) & 0xff))
            PC.increment()

          case (0xe7) => // RST 20H
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(PC)
            PC(0x20)

          case (0xe8) => // RET PE
            if (testFlag(F, FLAG_P)) {
              CHECK_BREAK_WORD(SP)
              POP(PC)
              addTStates(11)
            } else {
              addTStates(5)
            }

          case (0xe9) => // JP (HL)
            addTStates(4)
            PC(HL)

          case (0xea) => // JP PE,nnnn
            JPC(testFlag(F, FLAG_P))

          case (0xeb) => // EX DE,HL
            addTStates(4)
            val temp: Int = HL
            HL(DE)
            DE(temp)

          case (0xec) => // CALL PE,nnnn
            CALLC(testFlag(F, FLAG_P))

          // *************************************************************************************** ED
          case (0xed) => // ED PREFIX
            INCR(1)
            val op: Int = MMU.get8(PC)
            PC.increment()
            op match {
              case (0x40) => // IN B,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                B(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x41) => // OUT (C),B
                addTStates(12)
                MMU.out8(C, B)

              case (0x42) => // SBC HL, BC
                addTStates(15)
                SBC(HL, BC)

              case (0x43) => // LD (nnnn),BC
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                MMU.put16(temp, BC)
                PC(PC + 2)

              case (0x44) => // NEG
                NEG

              case (0x4C) => // NEG
                NEG

              case (0x54) => // NEG
                NEG

              case (0x64) => // NEG
                NEG

              case (0x6C) => // NEG
                NEG

              case (0x74) => // NEG
                NEG

              case (0x7C) => // NEG
                NEG

              case (0x45) => // RETN
                RETN

              case (0x55) => // RETN
                RETN

              case (0x5D) => // RETN
                RETN

              case (0x65) => // RETN
                RETN

              case (0x6D) => // RETN
                RETN

              case (0x75) => // RETN
                RETN

              case (0x7D) => // RETN
                RETN

              case (0x46) => // IM 0
                addTStates(8) // Interrupt mode 0

              case (0x47) => // LD I,A
                addTStates(9)
                I((I & 0xff) | (AF & ~0xff))

              case (0x48) => // IN C,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                C(temp)
                AF((AF & ~0xfe) | rotateShiftTable(temp & 0xff))

              case (0x49) => // OUT (C),C
                addTStates(12)
                MMU.out8(C, C)

              case (0x4a) => // ADC HL,BC
                addTStates(15)
                ADC(HL, BC)

              case (0x4b) => // LD BC,(nnnn)
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                BC(MMU.get16(temp))
                PC(PC + 2)

              case (0x4d) => // RETI
                addTStates(14)
                IFF(IFF | IFF >> 1)
                CHECK_BREAK_WORD(SP)
                POP(PC)

              case (0x4f) => // LD R,A
                addTStates(9)
                IR((IR & ~0xff) | ((AF >> 8) & 0xff))

              case (0x50) => // IN D,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                D(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x51) => // OUT (C),D
                addTStates(12)
                MMU.out8(C, D)

              case (0x52) => // SBC HL,DE
                addTStates(15)
                SBC(HL, DE)

              case (0x53) => // LD (nnnn),DE
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                MMU.put16(temp, DE)
                PC(PC + 2)

              case (0x56) => // IM 1
                addTStates(8)

              case (0x57) => // LD A,I
                addTStates(9)
                AF((AF & 0x29) | (IR & ~0xff) | ((IR >> 8) & 0x80) | ({
                  if ((IR & ~0xff) == 0) 1 else 0
                } << 6) | ((IFF & 2) << 1))

              case (0x58) => // IN E,(C)
                addTStates(12)
                val temp: Int = MMU.in8((C))
                E(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x59) => // OUT (C),E
                addTStates(12)
                MMU.out8(C, E)

              case (0x5a) => // ADC HL,DE
                addTStates(15)
                ADC(HL, DE)

              case (0x5b) => // LD DE,(nnnn)
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                DE(MMU.get16(temp))
                PC(PC + 2)

              case (0x5e) => // IM 2
                addTStates(8)

              case (0x5f) => // LD A,R
                addTStates(9)
                AF((AF & 0x29) | ((IR & 0xff) << 8) | (IR & 0x80) |
                  ({
                    if ((IR & 0xff) == 0) 1 else 0
                  } << 6) | ((IFF & 2) << 1))

              case (0x60) => // IN H,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                H(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x61) => // OUT (C),H
                addTStates(12)
                MMU.out8(C, H)

              case (0x62) => // SBC HL,HL
                addTStates(15)
                val sum: Int = HL - HL - {
                  if (testFlag(F, FLAG_C)) 1 else 0
                }
                AF((AF & 0xff) | ({
                  if ((sum & 0xffff) == 0) 1 else 0
                } << 6) | cbits2Z80DupTable((sum >> 8) & 0x1ff))
                HL(sum)

              case (0x63) => // LD (nnnn),HL
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                MMU.put16(temp, HL)
                PC(PC + 2)

              case (0x67) => // RRD
                addTStates(18)
                val temp: Int = MMU.get8(HL)
                val acu: Int = A
                MMU.put8(HL, HIGH_DIGIT(temp) | (LOW_DIGIT(acu) << 4))
                AF(rrdrldTable((acu & 0xf0) | LOW_DIGIT(temp)) | (AF & 1))

              case (0x68) => // IN L,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                L(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x69) => // OUT (C),L
                addTStates(12)
                MMU.out8(C, L)

              case (0x6a) => // ADC HL,HL
                addTStates(15)
                val sum: Int = HL + HL - {
                  if (testFlag(F, FLAG_C)) 1 else 0
                }
                AF((AF & ~0xff) | ({
                  if ((sum & 0xffff) == 0) 1 else 0
                } << 6) | cbits2Z80DupTable(sum >> 8))
                HL(sum)

              case (0x6b) => // LD HL,(nnnn)
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                HL(MMU.get16(temp))
                PC(PC + 2)

              case (0x6f) => // RLD
                addTStates(18)
                val temp: Int = MMU.get8(HL)
                val acu: Int = A
                MMU.put8(HL, (LOW_DIGIT(temp) << 4) | LOW_DIGIT(acu))
                AF(rrdrldTable((acu & 0xf0) | HIGH_DIGIT(temp)) | (AF & 1))

              case (0x70) => // IN (C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x71) => // OUT (C),0
                addTStates(12)
                MMU.out8(C, UByte(0))

              case (0x72) => // SBC HL,SP
                addTStates(15)
                SBC(HL, SP)

              case (0x73) => // LD (nnnn),SP
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                MMU.put16(temp, SP)
                PC(PC + 2)

              case (0x78) => // IN A,(C)
                addTStates(12)
                val temp: Int = MMU.in8(C)
                A(temp)
                AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

              case (0x79) => // OUT (C),A
                addTStates(12)
                MMU.out8(C, A)

              case (0x7a) => // ADC HL,SP
                addTStates(15)
                ADC(HL, SP)

              case (0x7b) => // LD SP,(nnnn)
                addTStates(20)
                val temp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(temp)
                SP(MMU.get16(temp))
                PC(PC + 2)

              case (0xa0) => // LDI
                addTStates(16)
                // CHECK_BREAK_TWO_BYTES - HL & DE
                var acu: Int = MMU.get8(HL)
                HL.increment()
                MMU.put8(DE, acu)
                DE.increment()
                acu = acu + A
                AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | ({
                  BC.decrement()
                  if ((BC & 0xffff) != 0) 1 else 0
                } << 2))

              case (0xa1) => // CPI
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: UInt = A.get8
                val temp: UInt = HL.get16
                HL.increment()
                val sum: UInt = acu - temp
                val cbits: UInt = acu ^ temp ^ sum
                AF((AF & ~0xfe) | (sum & 0x80) | {
                  if (((sum & 0xff) << 6) == 0) 1 else 0
                } |
                  (((sum - ((cbits & 16) >> 4)) & 2) << 4) | (cbits & 16) |
                  ((sum - ((cbits >> 4) & 1)) & 8) | {
                  BC.decrement()
                  if ((BC & 0xffff) != 0) 1 else 0
                } << 2 | 2)
                if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)


              /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
                NF flag A is copy of bit 7 of the value read from or written to an I/O port.
                INI/INIR/IND/INDR use the C flag in stead of the L register. There is a
                catch though, because not the value of C is used, but C + 1 if it's INI/INIR or
                C - 1 if it's IND/INDR. So, first of all INI/INIR:
                    HF and CF Both set if ((HL) + ((C + 1) & 255) > 255)
                    PF The parity of (((HL) + ((C + 1) & 255)) & 7) xor B)                      */
              case (0xa2) => // INI
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: Int = MMU.in8(C)
                MMU.put8(HL, acu)
                HL.increment()
                val temp: Int = B
                BC(BC - 0x100)
                INOUTFLAGS_NONZERO((C + 1) & 0xff, acu, temp)

              /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
  NF flag A is copy of bit 7 of the value read from or written to an I/O port.
  And now the for OUTI/OTIR/OUTD/OTDR instructions. Take state of the L
  after the increment or decrement of HL; add the value written to the I/O port
  to; call that k for now. If k > 255, then the CF and HF flags are set. The PF
  flags is set like the parity of k bitwise and'ed with 7, bitwise xor'ed with B.
  HF and CF Both set if ((HL) + L > 255)
  PF The parity of ((((HL) + L) & 7) xor B)                                       */
              case (0xa3) => // OUTI
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: Int = MMU.get8(HL)
                MMU.out8(C, UByte(acu.byteValue()))
                HL.increment()
                val temp: Int = B
                BC(BC - 0x100)
                INOUTFLAGS_NONZERO(L, acu, temp)

              case (0xa8) => // LDD
                addTStates(16)
                // CHECK_BREAK_TWO_BYTES HL,DE
                var acu: Int = MMU.get8(HL)
                HL.decrement()
                MMU.put8(DE, acu)
                DE.decrement()
                acu = acu + A
                AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | {
                  BC.decrement()
                  if ((BC & 0xffff) != 0) 1 else 0
                } << 2)

              case (0xa9) => // CPD
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: UInt = A.get8
                val temp: UInt = MMU.get8(HL)
                HL.decrement()
                val sum: UInt = acu - temp
                val cbits: UInt = acu ^ temp ^ sum
                AF(
                  (AF & ~0xfe) | (sum & 0x80) | ({
                    if ((sum & 0xff) == 0) 1 else 0
                  } << 6) |
                    (((sum - ((cbits & 16) >> 4)) & 2) << 4) | (cbits & 16) |
                    ((sum - ((cbits >> 4) & 1)) & 8) | {
                    BC.decrement()
                    if ((BC & 0xffff) != 0) 1 else 0
                  } << 2 | 2
                )
                if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)

              /*  SF, ZF, YF, XF flags are affected by decreasing register B, as in DEC B.
  NF flag A is copy of bit 7 of the value read from or written to an I/O port.
  INI/INIR/IND/INDR use the C flag in stead of the L register. There is a
  catch though, because not the value of C is used, but C + 1 if it's INI/INIR or
  C - 1 if it's IND/INDR. And last IND/INDR:
      HF and CF Both set if ((HL) + ((C - 1) & 255) > 255)
      PF The parity of (((HL) + ((C - 1) & 255)) & 7) xor B)                      */
              case (0xaa) => // IND
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: Int = MMU.in8(C)
                MMU.put8(HL, acu)
                HL.decrement()
                val temp: Int = B
                BC(BC - 0x100)
                INOUTFLAGS_NONZERO((C - 1) & 0xff, acu, temp)

              case (0xab) => // OUTD
                addTStates(16)
                CHECK_BREAK_BYTE(HL)
                val acu: Int = MMU.get8(HL)
                MMU.out8(C, acu)
                HL.decrement()
                val temp: Int = B
                BC(BC - 0x100)
                INOUTFLAGS_NONZERO(L, acu, temp)

              case (0xb0) => // LDIR
                addTStates(-5)
                // use local variable - quicker
                var bc: Int = BC
                var acu: Int = 0
                if (bc == 0) bc = 0x10000
                do {
                  addTStates(21)
                  INCR(2)
                  // CHECK_BREAK_TWO_BYTES(HL,DE)
                  acu = MMU.get8(HL)
                  HL.increment()
                  MMU.put8(DE, acu)
                  DE.increment()
                } while ( {
                  bc -= 1
                  bc != 0
                })
                acu += A
                BC(0) // BC will be zero
                AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4))

              case (0xb1) => // CPIR
                addTStates(-5)
                val acu: Int = A
                var bc: Int = BC
                var temp: Int = 0
                var sum: Int = 0
                var op: Int = 0

                if (bc == 0) bc = 0x10000
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  temp = MMU.get8(HL)
                  HL.increment()
                  bc -= 1
                  op = {
                    if (bc != 0) 1 else 0
                  }
                  sum = acu - temp

                } while (bc != 0 && sum != 0)
                val cbits = acu ^ temp ^ sum
                BC(0)
                val nc = {
                  if ((sum & 0xff) == 0) 1 else 0
                }
                AF((AF & ~0xfe) | (sum & 0x80) | (nc << 6) |
                  (((sum - ((cbits & 16) >> 4)) & 2) << 4) |
                  (cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
                  op << 2 | 2)
                if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)

              case (0xb2) => // INIR
                addTStates(-5)
                var temp: Int = {
                  if (B.intValue == 0) 0x100 else B
                }
                var acu: Int = 0
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  acu = MMU.in8(C)
                  MMU.put8(HL, acu)
                  HL.increment()

                } while ( {
                  temp -= 1
                  temp != 0
                })
                temp = B
                B(0)
                INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)

              case (0xb3) => // OTIR
                addTStates(-5)
                var temp = B.get8.intValue
                var acu = 0
                if (temp == 0) temp = 0x100
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  acu = MMU.get8(HL)
                  MMU.out8(C, acu)
                  HL.increment()
                } while ( {
                  temp -= 1
                  temp != 0
                })
                temp = B
                B(0)
                INOUTFLAGS_ZERO(L, acu, temp)


              case (0xb8) => // LDDR
                addTStates(-5)
                var count: Int = BC
                var acu: Int = 0
                if (count == 0) count = 0x10000
                BC(0)
                do {
                  addTStates(21)
                  INCR(2)
                  // CHECK_BREAK_TWO_BYTES(HL,DE)
                  acu = MMU.get8(HL)
                  HL.decrement()
                  MMU.put8(DE, acu)
                  DE.decrement()
                } while ( {
                  count -= 1
                  count != 0
                })
                acu += A.get8
                AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4))

              case (0xb9) => // CPDR
                addTStates(-5)
                val acu: Int = A
                var bc: Int = BC
                var temp: Int = 0
                var sum: Int = 0
                var op: Int = 0

                if (bc == 0) bc = 0x10000
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  temp = MMU.get8(HL)
                  HL.decrement()
                  bc -= 1
                  op = {
                    if (bc != 0) 1 else 0
                  }
                  sum = acu - temp

                } while (bc != 0 && sum != 0)
                val cbits = acu ^ temp ^ sum
                BC(0)
                val nc = {
                  if ((sum & 0xff) == 0) 1 else 0
                }
                AF((AF & ~0xfe) | (sum & 0x80) | (nc << 6) |
                  (((sum - ((cbits & 16) >> 4)) & 2) << 4) |
                  (cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
                  op << 2 | 2)
                if ((sum & 15) == 8 && (cbits & 16) != 0) AF(AF & ~8)

              case (0xba) => // INDR
                addTStates(-5)
                var temp: Int = {
                  if (B.intValue == 0) 0x100 else B
                }
                var acu: Int = 0
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  acu = MMU.in8(C)
                  MMU.put8(HL, acu)
                  HL.decrement()

                } while ( {
                  temp -= 1
                  temp != 0
                })
                temp = B
                B(0)
                INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)

              case (0xbb) => // OTDR
                addTStates(-5)
                var temp: Int = {
                  if (B.intValue == 0) 0x100 else B
                }
                var acu: Int = 0
                do {
                  addTStates(21)
                  INCR(1)
                  CHECK_BREAK_BYTE(HL)
                  acu = MMU.in8(C)
                  MMU.put8(HL, acu)
                  HL.decrement()

                } while ( {
                  temp -= 1
                  temp != 0
                })
                temp = B
                B(0)
                INOUTFLAGS_ZERO((C + 1) & 0xff, acu, temp)

              case _ =>
            }

          case (0xee) => // XOR nn
            addTStates(7)
            AF(xororTable(((AF >> 8) ^ MMU.get8(PC)) & 0xff))
            PC.increment()

          case (0xef) => // RST 28H
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(PC)
            PC(0x28)

          case (0xf0) => // RET P
            if (testFlag(F, FLAG_S)) {
              addTStates(5)
            } else {
              CHECK_BREAK_WORD(SP)
              POP(PC)
              addTStates(11)
            }

          case (0xf1) => // POP AF
            addTStates(10)
            CHECK_BREAK_WORD(SP)
            POP(AF)

          case (0xf2) => // JP P,nnnn
            JPC(!testFlag(F, FLAG_S))

          case (0xf3) => // DI
            addTStates(4)
            IFF(0)

          case (0xf4) => // CALL P,nnnn
            CALLC(!testFlag(F, FLAG_S))

          case (0xf5) => // PUSH AF
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(AF)

          case (0xf6) => // OR nn
            addTStates(7)
            AF(xororTable(((AF >> 8) ^ MMU.get8(PC)) & 0xff))
            PC.increment()

          case (0xf7) => // RST 30H
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(PC)
            PC(0x30)

          case (0xf8) => // RET M
            if (testFlag(F, FLAG_S)) {
              CHECK_BREAK_WORD(SP)
              POP(PC)
              addTStates(11)
            } else {
              addTStates(5)
            }

          case (0xf9) => // LD SP,HL
            addTStates(6)
            SP(HL)

          case (0xfa) => // JP M,nnnn
            JPC(testFlag(F, FLAG_S))

          case (0xfb) => // EI
            addTStates(4)
            IFF(3)

          case (0xfc) => // CALL M, nnnn
            CALLC(testFlag(F, FLAG_S))


          // ************************************************************************************ FD
          case (0xfd) => // FD Prefix
            INCR(1)
            val op: Int = MMU.get8(PC)
            PC.increment()
            op match {
              case (0x09) => // ADD IY,BC
                addTStates(15)
                ADD(IY, BC)

              case (0x19) => // ADD IY,DE
                addTStates(15)
                ADD(IY, DE)

              case (0x21) => // LD IY,nnnn
                addTStates(14)
                IY(MMU.get16(PC))
                PC(PC + 2)

              case (0x22) => // LD (nnnn),IY
                addTStates(20)
                val temp: Int = MMU.get16(PC)
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
                addTStates(9)
                IYH(MMU.get8(PC))
                PC.increment()

              case (0x29) => // ADD IY,IY
                addTStates(15)
                val sum: Int = IY + IY
                AF((AF & ~0x3b) | cbitsDup16Table(sum >> 8))
                IY(sum)

              case (0x2a) => // LD IY,(nnnn)
                addTStates(20)
                val tmp: Int = MMU.get16(PC)
                CHECK_BREAK_WORD(tmp)
                IY(MMU.get16(tmp))
                PC(PC + 2)

              case (0x2b) => // DEC IY
                addTStates(10)
                IY.decrement()

              case (0x2c) => // INC IYL
                addTStates(9)
                IYL.increment()
                AF((AF & ~0xfe) | incZ80Table(IYL))

              case (0x2d) => // DEC IYL
                addTStates(9)
                IYL.decrement()
                AF((AF & ~0xfe) | decZ80Table(IYL))

              case (0x2e) => // LD IYL,nn
              case (0x34) => // INC (IY+dd)
                addTStates(23)
                INCIDXdd(IY)

              case (0x35) => // DEC (IY+dd)
                addTStates(23)
                DECIDXdd(IY)

              case (0x36) => // LD (IY+dd),nn
              case (0x39) => // ADD IY,SP
                addTStates(15)
                val sum: Int = IY + SP
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
                LDIDXdd(B, IY)

              case (0x4c) => // LD C,IYH
                addTStates(9)
                C(IYH)

              case (0x4d) => // LD C,IYL
                addTStates(9)
                C(IYL)

              case (0x4e) => // LD C,(IY+dd)
                addTStates(19)
                LDIDXdd(C, IY)

              case (0x54) => // LD D,IYH
                addTStates(9)
                D(IYH)

              case (0x55) => // LD D,IYL
                addTStates(9)
                D(IYL)

              case (0x56) => // LD D,(IY+dd)
                addTStates(19)
                LDIDXdd(D, IY)

              case (0x5c) => // LD E,IYH
                addTStates(9)
                E(IYH)

              case (0x5d) => // LD E,IYL
                addTStates(9)
                E(IYL)

              case (0x5e) => // LD E,(IY+dd)
                addTStates(19)
                LDIDXdd(E, IY)

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
                LDIDXdd(H, IY)

              case (0x67) => // LD IYH,A
                addTStates(9)
                IYH(A)

              case (0x68) => // LD IYL,B
                addTStates(9)
                IYL(B)

              case (0x69) => // LD IYL,C
                addTStates(9)
                IYL(C)

              case (0x6a) => // LD IYL,D
                addTStates(9)
                IYL(D)

              case (0x6b) => // LD IYL,E
                addTStates(9)
                IYL(E)

              case (0x6c) => // LD IYL,IYH
                addTStates(9)
                IYL(IYH)

              case (0x6e) => // LD L,(IY+dd)
                addTStates(19)
                LDIDXdd(L, IY)

              case (0x6f) => // LD IYL,A
                addTStates(9)
                IYL(A)

              case (0x70) => // LD (IY+dd),B
                addTStates(19)
                LDIDXdd(IY, B)

              case (0x71) => // LD (IY+dd),C
                addTStates(19)
                LDIDXdd(IY, C)

              case (0x72) => // LD (IY+dd),D
                addTStates(19)
                LDIDXdd(IY, D)

              case (0x73) => // LD (IY+dd),E
                addTStates(19)
                LDIDXdd(IY, E)

              case (0x74) => // LD (IY+dd),H
                addTStates(19)
                LDIDXdd(IY, H)

              case (0x75) => // LD (IY+dd),L
                addTStates(19)
                LDIDXdd(IY, L)

              case (0x77) => // LD (IY+dd),A
                addTStates(19)
                LDIDXdd(IY, A)

              case (0x7c) => // LD A,IYH
                addTStates(9)
                A(IYH)

              case (0x7d) => // LD A,IYL
                addTStates(9)
                A(IYL)

              case (0x7e) => // LD A,(IY+dd)
                addTStates(19)
                LDIDXdd(A, IY)

              case (0x84) => // ADD A,IYH
                addTStates(9)
                val tmp: Int = IYH
                val acu: Int = A
                val sum: Int = acu + tmp
                AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)


              case (0x85) => // ADD A,IYL
                addTStates(9)
                val tmp: Int = IYL
                val acu: Int = A
                val sum: Int = acu + tmp
                AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)


              case (0x86) => // ADD A,(IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: Int = MMU.get8(adr)
                val acu: Int = A
                val sum: Int = acu + temp
                AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)


              case (0x8c) => // ADC A,IYH
                addTStates(9)
                val tmp: Int = IYH
                val acu: Int = A
                val sum: Int = acu + tmp + {
                  if (testFlag(F, FLAG_C)) 1 else 0
                }
                AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)

              case (0x8d) => // ADC A,IYL
                addTStates(9)
                val tmp: Int = IYL
                val acu: Int = A
                val sum: Int = acu + tmp + {
                  if (testFlag(F, FLAG_C)) 1 else 0
                }
                AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)

              case (0x8e) => // ADC A,(IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: Int = MMU.get8(adr)
                val acu: Int = A
                val sum: Int = acu + temp + {
                  if (testFlag(F, FLAG_C)) 1 else 0
                }
                AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)

              case (0x96) => // SUB (IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: UByte = MMU.get8(adr)
                val acu: UByte = A
                val sum: UInt = acu - temp
                AF((addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

              case (0x94) => // SUB IYH
                addTStates(9)
                SUBIDX(IYH)

              case (0x9c) => // SBC A,IYH
                addTStates(9)
                SBCAIDX(A, IYH)

              case (0x95) => // SUB IYL
                addTStates(9)
                SUBIDX(IYL)

              case (0x9d) => // SBC A,IYL
                addTStates(9)
                SBCAIDX(A, IYL)

              case (0x9e) => // SBC A,(IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: UByte = MMU.get8(adr)
                val acu: UByte = A
                val sum: UInt = acu - temp - {
                  if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
                }
                AF((addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

              case (0xa4) => // AND IYH
                addTStates(9)
                AF(andTable(((AF & IY) >> 8) & 0xff))

              case (0xa5) => // AND IYL
                addTStates(9)
                AF(andTable(((AF >> 8) & IY) & 0xff))

              case (0xa6) => // AND (IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                AF(andTable(((AF >> 8) & MMU.get8(adr)) & 0xff))

              case (0xac) => // XOR IYH
                addTStates(9)
                AF(xororTable(((AF ^ IY) >> 8) & 0xff))

              case (0xad) => // XOR IYL
                addTStates(9)
                AF(xororTable(((AF >> 8) ^ IY) & 0xff))

              case (0xae) => // XOR (IY+dd)
                addTStates(19)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                AF(xororTable(((AF >> 8) ^ MMU.get8(adr)) & 0xff))

              case (0xb4) => // OR IYH
                addTStates(9)
                AF(xororTable(((AF | IY) >> 8) & 0xff))

              case (0xb5) => // OR IYL
                addTStates(9)
                AF(xororTable(((AF >> 8) | IY) & 0xff))

              case (0xbc) => // CP IYH
                addTStates(9)
                val temp: UInt = IYH.toUInt
                AF((AF & ~0x28) | (temp & 0x28))
                val acu: UInt = A.toUInt
                val sum: UInt = acu - temp
                AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))

              case (0xbd) => // CP IYL
                addTStates(9)
                val temp: UInt = IYL.toUInt
                AF((AF & ~0x28) | (temp & 0x28))
                val acu: UInt = A.toUInt
                val sum: UInt = acu - temp
                AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))


              case (0xbe) => // CP (IY+dd)
                addTStates(9)
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                CHECK_BREAK_BYTE(adr)
                val temp: UInt = MMU.get8(adr)
                AF((AF & ~0x28) | (temp & 0x28))
                val acu: UInt = A.toUInt
                val sum: UInt = acu - temp
                AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))


              case (0xcb) => // ******************************************************** FD CB Prefix
                val adr: Int = IY + MMU.get8(PC)
                PC.increment()
                var acu: Int = 0
                var cbits: Int = 0
                var tmp: Int = 0
                val op: Int = MMU.get8(PC)
                (op & 7) match {
                  case 0 =>
                    PC.increment()
                    acu = B
                  case 1 =>
                    PC.increment()
                    acu = C
                  case 2 =>
                    PC.increment()
                    acu = D
                  case 3 =>
                    PC.increment()
                    acu = E
                  case 4 =>
                    PC.increment()
                    acu = H
                  case 5 =>
                    PC.increment()
                    acu = L
                  case 6 =>
                    CHECK_BREAK_BYTE(adr)
                    PC.increment()
                    acu = MMU.get8(adr)
                  case 7 =>
                    PC.increment()
                    acu = A
                  case _ =>
                }
                (op & 0xc0) match {

                  case (0x00) => // shift/rotate
                    (op & 0x38) match {
                      case (0x00) => // RLC
                        tmp = (acu << 1) | (acu >> 7)
                        cbits = tmp & 1
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x08) => // RRC
                        tmp = (acu >> 1) | (acu << 7)
                        cbits = tmp & 0x80
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x10) => // RL
                        tmp = (acu << 1) | {
                          if (testFlag(F, FLAG_C)) 1 else 0
                        }
                        cbits = acu & 0x80
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x18) => // RR
                        tmp = (acu >> 1) | ({
                          if (testFlag(F, FLAG_C)) 1 else 0
                        } << 7)
                        cbits = acu & 1
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x20) => // SLA
                        tmp = acu << 1
                        cbits = acu & 0x80
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x28) => // SRA
                        tmp = (acu >> 1) | (acu & 0x80)
                        cbits = acu & 1
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x30) => // SLIA
                        tmp = (acu << 1) | 1
                        cbits = acu & 0x80
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case (0x38) => // SRL
                        tmp = acu >> 1
                        cbits = acu & 1
                        AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
                          if (cbits == 0) 0 else 1
                        })

                      case _ =>
                    }

                  case (0x40) => // BIT
                    addTStates(20)
                    if ( {
                      if ((acu & (1 << ((op >> 3) & 7))) != 0)
                        true
                      else false
                    }
                    )
                      AF((AF & ~0xfe) | 0x10 | ({
                        if ((op & 0x38) == 0x380) 1 else 0
                      } << 7))
                    else
                      AF(AF | (acu & 0x28))
                    tmp = acu

                  case (0x80) => // RES
                    addTStates(23)
                    tmp = acu & ~(1 << ((op >> 3) & 7))

                  case (0xc0) => // SET
                    addTStates(23)
                    tmp = acu | ( 1<< ((op >> 3) & 7))

                  case _ =>
                }
                (op & 7) match {
                  case 0 =>
                    B(tmp)
                  case 1 =>
                    C(tmp)
                  case 2 =>
                    D(tmp)
                  case 3 =>
                    E(tmp)
                  case 4 =>
                    H(tmp)
                  case 5 =>
                    L(tmp)
                  case 6 =>
                    MMU.put8(adr,UByte(tmp.byteValue()))

                  case 7 =>
                    A(tmp)

                  case _ =>
                }
              case (0xe1) => // POP IY
                addTStates(14)
                CHECK_BREAK_WORD(SP)
                POP(IY)

              case (0xe3) => // EX (SP),IY
                addTStates(23)
                CHECK_BREAK_WORD(SP)
                val tmp:Int = IY
                POP(IY)
                PUSH(tmp)

              case (0xe5) => // PUSH IY
                addTStates(15)
                CHECK_BREAK_WORD(SP -2)
                PUSH(IY)

              case (0xe9) => // JP (IY)
                addTStates(8)
                PC(IY)

              case (0xf9) => // LD SP,IY
                addTStates(10)
                SP(IY)

              case _ =>
                PC.decrement()
            }

          case (0xfe) => // CP nn
            addTStates(7)
            val temp: UInt = MMU.get8(PC)
            PC.increment()
            F((F & ~0x28) | (temp & 0x28))
            val acu: UInt = A.get8
            val sum: UInt = acu - temp
            val cbits: UInt = acu ^ temp ^ sum
            F((F & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) |
              SET_PV(cbits) | cbits2Table(cbits & 0x1ff))

          case (0xff) => // RST 38H
            addTStates(11)
            CHECK_BREAK_WORD(SP - 2)
            PUSH(PC)
            PC(0x38)

        }


        SimTimer.sim_interval = SimTimer.sim_interval - 1


      } // end SwitchCPUNow

      // TODO simulation halted
      onHalt()
    } catch {
      case t: Throwable =>
        onHalt()
        throw t
    }
  }

  override def onHalt(): Unit = {
    Utils.outln(s"$name: Halted.")
    Utils.outln(showRegisters())
    Utils.outln(showFlags())
  }

  @inline
  private final def addTStates(x: Long): Unit = {
    tStates = tStates + x
  }

  @inline
  private final def INCR(count: Int): Unit = {
    R.set8(UByte(((R.get8 & ~0x7f) | ((R.get8 + count) & 0x7f)).toByte)) // Increment R
  }

  @inline
  // TODO Implement memory break points.
  private final def CHECK_BREAK_BYTE(reg: Register16): Boolean = {
    false
  }

  @inline
  private final def CHECK_BREAK_BYTE(v: UShort): Boolean = false

  @inline // TODO Implement memory break points.
  private final def CHECK_BREAK_WORD(reg: Register16): Boolean = false

  @inline
  private final def CHECK_BREAK_BYTE(v: Int): Boolean = false

  @inline
  private final def CHECK_BREAK_WORD(addr: Int): Boolean = false

  @inline
  private final def HIGH_DIGIT(x: Int): Int = {
    (x >> 4) & 0xf
  }

  @inline
  private final def LOW_DIGIT(x: Int): Int = {
    x & 0xf
  }

  private final val incTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0) 1 else 0) << 4
      UByte(((temp & 0xa8) | (t1 | t2)).toByte)
    }
  }.toArray

  private final val incZ80Table: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0) 1 else 0) << 4
      val t3 = (if (temp == 0x80) 1 else 0) << 2
      UByte(((temp & 0xa8) | (t1 | (t2 | t3))).toByte)
    }
  }.toArray


  private final val parityTable: Array[UByte] = {
    for (i <- 0 to 255) yield {
      if ((((i & 1) + ((i & 2) >> 1) + ((i & 4) >> 2) + ((i & 8) >> 3) +
        ((i & 16) >> 4) + ((i & 32) >> 5) + ((i & 64) >> 6) + ((i & 128) >> 7)) % 2) != 0)
        UByte(0) else UByte(4)
    }
  }.toArray

  private final val decTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0xf) 1 else 0) << 4
      UByte((((temp & 0xa8) | (t1 | t2)) | 2).toByte)
    }
  }.toArray

  private final val decZ80Table: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = (if ((temp & 0xff) == 0) 1 else 0) << 6
      val t2 = (if ((temp & 0xf) == 0xf) 1 else 0) << 4
      val t3 = (if (temp == 0x7f) 1 else 0) << 2
      UByte(((temp & 0xa8) | (t1 | (t2 | t3)) | 2).toByte)
    }
  }.toArray

  private final val cbitsTable: Array[UInt] = {
    for (cbits <- 0 to 511) yield {
      UInt(cbits & 0x10 | ((cbits >> 8) & 1))
    }
  }.toArray

  private final val cbitsZ80Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | (((cbits >> 6) ^ (cbits >> 5)) & 4) |
        ((cbits >> 8) & 1)).toByte)
    }
  }.toArray

  private final val cbitsDup8Table: Array[UShort] = {
    for (cbits <- 0 to 511) yield {
      val t1 = if ((cbits & 0xff) == 0) 1 else 0
      UShort(((cbits & 0x10) | ((cbits >> 8) & 1) | ((cbits & 0xff) << 8) | (cbits & 0xa8) | t1 << 6).toShort)
    }
  }.toArray

  private final val cbitsZ80DupTable: Array[UShort] = {
    for (cbits <- 0 to 511) yield {
      val t1 = if ((cbits & 0xff) == 0) 1 else 0
      UShort(((cbits & 0x10) | (((cbits >> 6) ^ (cbits >> 5)) & 4) |
        ((cbits >> 8) & 1) | (cbits & 0xa8)).toShort)
    }
  }.toArray

  private final val cbitsDup16Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | ((cbits >> 8) & 1) | (cbits & 0x28)).toByte)
    }
  }.toArray

  private final val cbits2Table: Array[UInt] = {
    for (cbits <- 0 to 511) yield {
      UInt(cbits & 0x10 | ((cbits >> 8) & 1) | 2)
    }
  }.toArray

  private final val cbits2Z80Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((((cbits >> 6) ^ (cbits >> 5)) & 4) | (cbits & 0x10) | 2 | ((cbits >> 8) & 1)).toByte)
    }
  }.toArray

  private final val cbits2Z80DupTable: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((((cbits >> 6) ^ (cbits >> 5)) & 4) | (cbits & 0x10) | 2 | ((cbits >> 8) & 1) |
        (cbits & 0xa8)).toByte)
    }
  }.toArray

  private final val rrcaTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort((((temp & 1) << 15) | (sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private final val rraTable: Array[UShort] = {
    for (temp <- 0 to 255) yield {
      val sum = temp >> 1
      UShort(((sum << 8) | (sum & 0x28) | (temp & 1)).toShort)
    }
  }.toArray

  private final val addTable: Array[UShort] = {
    for (sum <- 0 to 511) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UShort((((sum & 0xff) << 8) | (sum & 0xa8) | (t1 << 6)).toShort)
    }
  }.toArray

  private final val subTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UShort((((sum & 0xff) << 8) | (sum & 0xa8) | (t1 << 6) | 2).toShort)
    }
  }.toArray

  private final val andTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if (sum == 0) 1 else 0
      UShort(((sum << 8) | (sum & 0xa8) | (t1 << 6) | 0x10 | parityTable(sum)).toShort)
    }
  }.toArray

  private final val xororTable: Array[UShort] = {
    for (sum <- 0 to 255) yield {
      val t1 = if (sum == 0) 1 else 0
      UShort(((sum << 8) | (sum & 0xa8) | (t1 << 6) | parityTable(sum)).toShort)
    }
  }.toArray

  @inline
  private final def PARITY(value: Int): UByte = parityTable(value & 0xff)

  private final val rotateShiftTable: Array[UByte] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UByte(((sum & 0xa8) | (t1 << 6) | PARITY(sum)).toByte)
    }
  }.toArray

  private final val negTable: Array[UByte] = {
    for (temp <- 0 to 255) yield {
      val t1 = if ((temp & 0x0f) != 0) 1 else 0
      val t2 = if (temp == 0x80) 1 else 0
      UByte(((t1 << 4) | (t2 << 2) | 2 | {
        if (temp != 0) 1 else 0
      }).toByte)
    }
  }.toArray

  private final val rrdrldTable: Array[UByte] = {
    for (acu <- 0 to 255) yield {
      val t1 = if ((acu & 0xff) == 0) 1 else 0
      UByte(((acu << 8) | (acu & 0xa8) | (t1 << 6) | parityTable(acu)).toByte)
    }
  }.toArray

  private final val cpTable: Array[UByte] = {
    for (sum <- 0 to 255) yield {
      val t1 = if ((sum & 0xff) == 0) 1 else 0
      UByte(((sum & 0x80) | (t1 << 6)).toByte)
    }
  }.toArray

  // Used in DAA
  private final val parity: Array[Boolean] = {
    for (i <- 0 to 255) yield {
      var bp = true
      for (j <- 0 to 7) {
        if ((i & (1 << j)) != 0) bp = !bp
      }
      bp
    }

  }.toArray

  @inline
  private final def SET_PV2(x: UByte, temp: Register8): UByte = {
    UByte(({
      if (temp.get8 == x) 1 else 0
    } << 2).toByte)
  }

  @inline
  private final def SET_PV2(x: Int, temp: Register8): UByte = SET_PV2(UByte(x.toByte), temp)

  @inline
  private final def SET_PV2(x: Int, temp: Int): UByte = {
    UByte(({
      if (temp == x) 1 else 0
    } << 2).toByte)
  }

  @inline
  private final def SET_PV(cbits: UInt): Int = (cbits >> 6) ^ (cbits >> 5) & 4

  @inline
  private final def POP(x: Register16): Unit = {
    val y = MMU.get8(SP)
    SP.increment()
    x(y + (MMU.get8(SP) << 8).shortValue)
    SP.increment()
  }

  @inline
  private final def PUSH(x: Register16): Unit = {
    PUSH(x.get16)
  }

  @inline
  private final def PUSH(x: Int): Unit = {
    SP.decrement()
    MMU.put8(SP, UByte((x >> 8).byteValue))
    SP.decrement()
    MMU.put8(SP, UByte((x & 0xff).byteValue))
  }


  @inline
  private final def JPC(cond: => Boolean): Unit = {
    addTStates(10)
    if (cond) {
      PC(MMU.get16(PC))
    } else PC(PC + 2)
  }

  @inline
  private final def CALLC(cond: => Boolean): Unit = {
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

  //**** Utility helpers *****
  @inline
  private final def ADDIDX(r1: Register8, r2: Register8): Unit = {
    val tmp: Int = r2
    val acu: Int = r1
    val sum: Int = acu + tmp
    AF((addTable(sum) | cbitsZ80Table(acu ^ tmp ^ sum)).intValue)
  }

  @inline
  private final def ADD(r1: Register16, r2: Register16): Unit = {
    val sum: Int = r1 + r2
    if (r1.nmenomic == r2.nmenomic) AF((AF & ~0x3b) | cbitsDup16Table(sum >> 8))
    else AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((r1 ^ r2 ^ sum) >> 8))
    r1(sum)
  }

  @inline
  private final def ADD(r1: Register8, r2: Register8): Unit = {
    val temp: UInt = r2.get8
    val acu: UInt = r1.get8
    val sum: UInt = acu + temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADD(r1: Register8, temp: UInt): Unit = {
    val acu: UInt = r1.get8
    val sum: UInt = acu + temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADC(r1: Register8, r2: Register8): Unit = {
    val temp: UInt = r2.get8
    val acu: UInt = r1.get8
    val sum: UInt = acu + temp + {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADC(r1: Register16, r2: Register16): Unit = {
    val sum: Int = r1 + r2 + {
      if (testFlag(F, FLAG_C)) 1 else 0
    }
    AF((AF & ~0xff) | ((sum >> 8) & 0xa8) | ({
      if ((sum & 0xffff) == 0) 1 else 0
    } << 6) | cbitsZ80Table((HL ^ BC ^ sum) >> 8))
    r1(sum)
  }

  @inline
  private final def ADCIDX(r1: Register8, r2: Register8): Unit = {
    val temp: Int = r2
    val acu: Int = r1
    val sum: Int = acu + temp + (if (testFlag(F, FLAG_C)) 1 else 0)
    AF((addTable(sum) | cbitsZ80Table(acu ^ temp ^ sum)).intValue)
  }

  @inline
  private final def SUB(r1: Register8): Unit = {
    val temp: UInt = r1.get8
    val acu: UInt = A.get8
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SUB(temp: UInt): Unit = {
    val acu: UInt = A.get8
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SUBIDX(r1: Register8): Unit = {
    val temp: UInt = r1.get8
    val acu: UInt = A.get8
    val sum: UInt = acu - temp
    AF((addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)
  }

  @inline
  private final def SBC(r1: Register8, r2: Register8): Unit = {
    val temp: UInt = r2.get8
    val acu: UInt = r1.get8
    val sum: UInt = acu - temp - {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SBC(r1: Register16, r2: Register16): Unit = {
    val sum: Int = r1 - r2 - (if (testFlag(F, FLAG_C)) 1 else 0)
    AF((AF & 0xff) | ((sum >> 8) & 0xa8) | ({
      if ((sum & 0xffff) == 0) 1 else 0
    } << 6) |
      cbits2Z80Table(((HL ^ BC ^ sum) >> 8) & 0x1ff))

    r1(sum)
  }

  @inline
  private final def SBCIDX(r1: Register8, r2: Register8): Unit = {
    val temp: UInt = r2.get8
    val acu: UInt = r1.get8
    val sum: UInt = acu - temp - (if (testFlag(F, FLAG_C)) UInt(1) else UInt(0))
    AF(addTable(sum & 0xff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff).intValue)
  }

  @inline
  private final def SBCAIDX(r1: Register8, r2: Register8): Unit = {
    val temp: UInt = r2.get8
    val acu: UInt = r1.get8
    val sum: UInt = acu - temp - (if (testFlag(F, FLAG_C)) UInt(1) else UInt(0))
    AF(addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff).intValue)
  }

  @inline
  private final def ICP(r1: Register8): Unit = {
    val temp: UInt = r1.get8
    val acu: UInt = A.get8
    AF(AF & ~0x28 | temp & 0x28)
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF((AF & 0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
  }

  @inline
  private final def LDIDXdd(r1: Register8, ridx: Register16): Unit = {
    val adr: Int = ridx + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    r1(MMU.get8(adr))
  }

  @inline
  private final def LDIDXdd(ridx: Register16, r1: Register8): Unit = {
    val adr = ridx + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    MMU.put8(adr, r1)
  }

  @inline
  private final def INC(r1: Register8): Unit = {
    r1.increment()
    AF((AF & ~0xfe) | incTable(r1) | SET_PV2(0x80, r1))
  }

  @inline
  private final def DEC(r1: Register8): Unit = {
    r1.decrement()
    AF((AF & ~0xfe) | decTable(r1) | SET_PV2(0x7f, r1))
  }

  @inline
  private final def INCIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    val temp: UByte = UByte((MMU.get8(adr) + UByte(1)).toByte)
    MMU.put8(adr, temp)
    AF((AF & ~0xfe) | incZ80Table(temp))
  }

  @inline
  private final def DECIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    val temp: UByte = UByte((MMU.get8(adr) - UByte(1)).toByte)
    MMU.put8(adr, temp)
    AF((AF & ~0xfe) | decZ80Table(temp & 0xff))
  }

  @inline
  private final def ANDIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    AF(andTable(((AF >> 8) & MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def XORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    AF(xororTable(((AF >> 8) ^ MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def ORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    AF(xororTable(((AF >> 8) | MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def CPIX8(r1: Register8): Unit = {
    val temp: UInt = r1.get8
    AF((AF & ~0x28) | (temp & 0x28))
    val acu: UInt = A.get8
    val sum: UInt = acu - temp
    AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))
  }

  @inline
  private final def CPIDXdd(r1: Register16): Unit = {
    val adr: Int = r1 + MMU.get8(PC)
    PC.increment()
    CHECK_BREAK_BYTE(adr)
    val temp: UInt = MMU.get8(adr)
    AF((AF & ~0x28) | (temp & 0x28))
    val acu: UInt = A.get8
    val sum: UInt = acu - temp
    AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))
  }

  @inline
  private final def NEG: Unit = {
    addTStates(8)
    val temp: Int = A
    AF((~(AF & 0xff00) + 1) & 0xff00)
    AF(AF | ((AF >> 8) & 0xa8) | ({
      if ((AF & 0xff00) == 0) 1 else 0
    } << 6) | negTable(temp))

  }

  @inline
  private final def RETN: Unit = {
    addTStates(14)
    IFF(IFF | (IFF >> 1))
    CHECK_BREAK_WORD(SP)
    POP(PC)
  }

  /*
  Macros for the IN/OUT instructions INI/INIR/IND/INDR/OUTI/OTIR/OUTD/OTDR
    Pre condition
        temp == value of register B at entry of the instruction
        acu == value of transferred byte (IN or OUT)
    Post condition
        F is set correctly
    Use INOUTFLAGS_ZERO(x) for INIR/INDR/OTIR/OTDR where
        x == (C + 1) & 0xff for INIR
        x == L              for OTIR and OTDR
        x == (C - 1) & 0xff for INDR
    Use INOUTFLAGS_NONZERO(x) for INI/IND/OUTI/OUTD where
        x == (C + 1) & 0xff for INI
        x == L              for OUTI and OUTD
        x == (C - 1) & 0xff for IND
*/
  @inline
  private final def INOUTFLAGS_NONZERO(x: Int, acu: Int, temp: Int): Unit = {
    INOUTFLAGS((C & 0xa8) | ({
      if (C.intValue == 0) 1 else 0
    } << 6), x, acu, temp)
  }

  @inline
  private final def INOUTFLAGS_ZERO(x: Int, acu: Int, temp: Int): Unit = {
    INOUTFLAGS(FLAG_Z, x, acu, temp)
  }

  @inline
  private final def INOUTFLAGS(syzx: Int, x: Int, acu: Int, temp: Int): Unit = {
    AF((AF & 0xff00) | syzx |
      ((acu & 0x80) >> 6) | {
      if ((acu + x) > 0xff) (FLAG_C | FLAG_H) else 0
    } |
      parityTable(((acu + x) & 7) ^ temp))
  }


  var i = 0
  while ( {
    i < 256
  }) {
    var bp = true
    var j = 0
    while ( {
      j < 8
    }) {
      if ((i & (1 << j)) != 0) bp = !bp

      {
        j += 1;
        j - 1
      }
    }
    parity(i) = bp

    {
      i += 1;
      i - 1
    }
  }
}
