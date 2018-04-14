package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}
import com.sim.{SimTimer, Utils}

import scala.annotation.switch

class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "Z80"
  override val MMU: Z80MMU = new Z80MMU(this)
  override val description: String = "Z80 CPU"

  override def init(): Unit = {} // TODO

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def handles(value: UInt): Boolean = ???

  var tStates: Long = 0L

  // Default timer interrupt handler routine
  val timerInterruptHandler: Int = 0xFC00

  // Default keyboard interrupt handler routine
  val keyboardInterruptHandler: Int = 0x0038

  override def createUnitOptions: Unit = {
    // Set up CPU common options.
    super.createUnitOptions

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("ALTAIRROM", "Enable Altair ROM.", value = false))
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Break on HALT instruction.", value = false))
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

  // Special Internal work register - used in some places to do alu ops
  // Not offical, and not exposed.
  private val W = new Register8(nmenomic = "W")

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

  // Z80 Flags
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

  override def runcpu(singleStep: Boolean, startAddr: UInt): Unit = {
    // Force the PC
    PC(startAddr.intValue)
    runcpu(singleStep)
  }

  override def runcpu(singleStep: Boolean = false): Unit = {

    try {
      // tStates contains the number of t-states executed.  1 t-state is executed in 1 microsecond
      // on a 1Mhz CPU.  tStates is used for real-time simulations.
      val sliceLength = 10L
      var tStatesInSlice: Long = sliceLength * clockFrequency // Number of t-states in 10mSec time-slice
      var startTime: Long = System.currentTimeMillis()
      var now: Long = 0L
      var execute: Boolean = true
      clockHasChanged = true
      tStates = 0L


      while (execute) {

        // If we are single stepping, the only do this one instruction.
        if (singleStep) execute = false

        if (machine.checkBreak(PC.toUInt) && lastBreak != PC.toUInt && !singleStep) {
          execute = false
          lastBreak = PC.toUInt
          Utils.outln(f"SIM: Break at: ${PC.intValue}%05X")

        } else {
          lastBreak = UInt(0)

          if (SimTimer.sim_interval <= 0) { // Check clock queue
            // sim_process_event()
            machine.eventQueue.processEvent()

            if (clockHasChanged) {
              clockHasChanged = false
              tStates = 0L
              startTime = System.currentTimeMillis()
              tStatesInSlice = sliceLength * clockFrequency
            }
          }

          // Quick check for special processing
          if (clockFrequency != 0 || timerInterrupt || keyboardInterrupt) {
            if (clockFrequency != 0 && (tStates >= tStatesInSlice)) {
              startTime += sliceLength
              tStates -= tStatesInSlice
              now = System.currentTimeMillis()
              if (startTime > now) Thread.sleep(0, (1000 * (startTime - now)).intValue())
            }

            if (timerInterrupt && (IFF & 1) != 0) {
              timerInterrupt = false
              IFF(0) // Disable Interrupts
              val currentOp = MMU.get8(PC)
              if ((currentOp == 0x76) && !stopOnHALT) {
                PUSH(PC + 1)
              } else {
                PUSH(PC)
              }
              PC(timerInterruptHandler)
            }
            if (keyboardInterrupt && (IFF & 1) != 0) {
              keyboardInterrupt = false
              IFF(0) // Disable Interrupts
              val currentOp = MMU.get8(PC)
              if ((currentOp == 0x76) && !stopOnHALT) {
                PUSH(PC + 1)
              } else {
                PUSH(PC)
              }
              PC(keyboardInterruptHandler)

            }


          }

          // Interrupted the sim
          if (com.sim.Console.userInterrupt) execute = false



          // Instruction execution

          INCR(1)
          val instr: Int = MMU.get8(PC)
          //if(PC.intValue == 0x0000) Utils.outln("*** CPU 0h ****")
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
              CHECK_LOG_BYTE(BC)
              MMU.put8(BC, A)

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
              CHECK_LOG_BYTE(BC)
              A(MMU.get8(BC))

            case (0x0b) => // DEC BC
              addTStates(6)
              BC.decrement()

            case (0x0c) => // INC C
              addTStates(4)
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
              BC(BC.get16 - 0x100)
              if ((BC.get16 & 0xff00) == 0) {
                // Jump
                addTStates(13)
                PC(PC.get16 + MMU.get8(PC) + 1)
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
              CHECK_LOG_BYTE(DE)
              MMU.put8(DE, A)

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
              CHECK_LOG_BYTE(DE)
              A(MMU.get8(DE))

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
              if ((F & FLAG_Z) != 0) { // Z flag
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
              CHECK_LOG_WORD(PC)
              val dest: Int = MMU.get16(PC)
              MMU.put16(dest, HL)
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
              val tmp1: UByte = A
              var tmp2 = 0
              val tmp3 = F & 1
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

            case (0x29) => // ADD HL,HL
              addTStates(11)
              ADD(HL, HL)

            case (0x2a) => // LD HL,(nnnn)
              addTStates(16)
              CHECK_LOG_WORD(PC)
              val fetch: Int = MMU.get16(PC)
              CHECK_LOG_WORD(fetch)
              HL(MMU.get16(fetch))
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
              CHECK_LOG_WORD(PC)
              val fetch: Int = MMU.get16(PC)
              CHECK_LOG_WORD(fetch)
              MMU.put8(fetch, A)
              PC(PC + 2)

            case (0x33) => // INC SP
              addTStates(6)
              SP.increment()

            case (0x34) => // INC (HL)
              addTStates(11)
              CHECK_LOG_BYTE(HL)
              val temp: UByte = UByte((MMU.get8(HL) + UByte(1)).byteValue)
              MMU.put8(HL, temp.intValue)
              AF((AF & ~0xfe) | incTable(temp) | SET_PV2(0x80, temp.intValue))

            case (0x35) => // DEC (HL)
              addTStates(11)
              CHECK_LOG_BYTE(HL)
              val temp: UByte = UByte((MMU.get8(HL) - UByte(1)).byteValue)
              MMU.put8(HL, temp.intValue)
              AF((AF & ~0xfe) | decTable(temp) | SET_PV2(0x7f, temp.intValue))

            case (0x36) => // LD (HL),nn
              addTStates(10)
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(tmp)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
              L(MMU.get8(HL))

            case (0x6f) => // LD L,A
              addTStates(4)
              L(A)

            case (0x70) => // LD (HL),B
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, B)

            case (0x71) => // LD (HL),C
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, C)

            case (0x72) => // LD (HL),D
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, D)

            case (0x73) => // LD (HL),E
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, E)

            case (0x74) => // LD (HL),H
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, H)

            case (0x75) => // LD (HL),L
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              MMU.put8(HL, L)


            case (0x76) => // HALT
              addTStates(4)
              PC(PC - 1)
              // Check stop on halt, otherwise sim_sleep
              if (stopOnHALT) execute = false
              else {
                SimTimer.sim_interval = 0
                // Only sleep if there are no interrupts pending
                if (!keyboardInterrupt && !timerInterrupt) Thread.sleep(0, 100) // 100 uSecs
              }


            case (0x77) => // LD (HL),A
              addTStates(7)
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
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
              CHECK_LOG_BYTE(HL)
              W(MMU.get8(HL))
              ADD(A,W)

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
              ADC(A, D)

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
              CHECK_LOG_BYTE(HL)
              W(MMU.get8(HL))
              ADC(A,W)

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
              CHECK_LOG_BYTE(HL)
              val temp: UByte = MMU.get8(HL)
              val acu: UByte = A.get8
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
              CHECK_LOG_BYTE(HL)
              W(MMU.get8(HL))
              SBC(A,W)

            case (0x9f) => // SBC A,A
              addTStates(4)
              val cbits: UInt = {
                if (testFlag(F, FLAG_C)) UInt(-1) else UInt(0)
              }
              AF(subTable(cbits & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))

            case (0xa0) => // AND B
              addTStates(4)
              AF(andTable(A.get8 & B.get8))

            case (0xa1) => // AND C
              addTStates(4)
              AF(andTable(A.get8 & C.get8))

            case (0xa2) => // AND D
              addTStates(4)
              AF(andTable(A.get8 & D.get8))

            case (0xa3) => // AND E
              addTStates(4)
              AF(andTable(A.get8 & E.get8))

            case (0xa4) => // AND H
              addTStates(4)
              AF(andTable(A.get8 & H.get8))

            case (0xa5) => // AND L
              addTStates(4)
              AF(andTable(A.get8 & L.get8))

            case (0xa6) => // AND (HL)
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              AF(andTable(A.get8 & MMU.get8(HL)))

            case (0xa7) => // AND A
              addTStates(4)
              AF(andTable(A.get8))

            case (0xa8) => // XOR B
              addTStates(4)
              AF(xororTable(A.get8 ^ B.get8))

            case (0xa9) => // XOR C
              addTStates(4)
              AF(xororTable(A.get8 ^ C.get8))

            case (0xaa) => // XOR D
              addTStates(4)
              AF(xororTable(A.get8 ^ D.get8))

            case (0xab) => // XOR E
              addTStates(4)
              AF(xororTable(A.get8 ^ E.get8))

            case (0xac) => // XOR H
              addTStates(4)
              AF(xororTable(A.get8 ^ H.get8))

            case (0xad) => // XOR L
              addTStates(4)
              AF(xororTable(A.get8 ^ L.get8))

            case (0xae) => // XOR (HL)
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              AF(xororTable(A.get8 ^ MMU.get8(HL)))

            case (0xaf) => // XOR A
              addTStates(4)
              AF(0x44)

            case (0xb0) => // OR B
              addTStates(4)
              AF(xororTable(A.get8 | B.get8))

            case (0xb1) => // OR C
              addTStates(4)
              AF(xororTable(A.get8 | C.get8))

            case (0xb2) => // OR D
              addTStates(4)
              AF(xororTable(A.get8 | D.get8))

            case (0xb3) => // OR E
              addTStates(4)
              AF(xororTable(A.get8 | E.get8))

            case (0xb4) => // OR H
              addTStates(4)
              AF(xororTable(A.get8 | H.get8))

            case (0xb5) => // OR L
              addTStates(4)
              AF(xororTable(A.get8 | L.get8))

            case (0xb6) => // OR (HL)
              addTStates(7)
              CHECK_LOG_BYTE(HL)
              AF(xororTable((AF >> 8) | MMU.get8(HL) & 0xff))

            case (0xb7) => // OR A
              addTStates(4)
              AF(xororTable((AF >> 8) & 0xff))

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
              CHECK_LOG_BYTE(HL)
              val temp: UByte = MMU.get8(HL)
              AF((AF & ~0x28) | (temp & 0x28))
              val acu: UByte = A.get8
              val sum: UInt = acu - temp
              val cbits: UInt = acu ^ temp ^ sum
              AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))

            case (0xbf) => // CP A
              addTStates(4)
              F((A & 0x28) | 0x42)

            case (0xc0) => // RET NZ
              if (testFlag(F, FLAG_Z)) addTStates(5)
              else {
                CHECK_LOG_WORD(SP)
                addTStates(11)
                POP(PC)
              }

            case (0xc1) => // POP BC
              addTStates(10)
              CHECK_LOG_WORD(SP)
              POP(BC)

            case (0xc2) => // JP NZ,nnnn
              JPC(!testFlag(F, FLAG_Z))

            case (0xc3) => // JP nnnn
              JPC(true)

            case (0xc4) => // CALL NZ,nnnn
              CALLC(!testFlag(F, FLAG_Z))

            case (0xc5) => // PUSH BC
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(BC)

            case (0xc6) => // ADD A,nn
              addTStates(7)
              W(MMU.get8(PC))
              PC.increment()
              ADD(A,W)

            case (0xc7) => // RST 0
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x0000)

            case (0xc8) => // RET Z
              if (testFlag(F, FLAG_Z)) {
                CHECK_LOG_WORD(SP)
                POP(PC)
                addTStates(11)
              } else {
                addTStates(5)
              }

            case (0xc9) => // RET
              addTStates(10)
              CHECK_LOG_WORD(SP)
              POP(PC)

            case (0xca) => // JP Z,nnnn
              JPC(testFlag(F, FLAG_Z))

            // ******************************************************************************** CB
            case (0xcb) => // CB prefix
              INCR(1)
              val adr: Int = HL.get16
              val op: Int = MMU.get8(PC)
              cbprefix(op, adr)

            case (0xcc) => // CALL Z,nnnn
              CALLC(testFlag(F, FLAG_Z))

            case (0xcd) => // CALL nnnn
              CALLC(true)

            case (0xce) => // ADC A,nn
              addTStates(7)
              W( MMU.get8(PC))
              PC.increment()
              ADC(A,W)

            case (0xcf) => // RST 8
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x0008)

            case (0xd0) => // RET NC
              if (testFlag(F, FLAG_C)) addTStates(5)
              else {
                CHECK_LOG_WORD(SP)
                POP(PC)
                addTStates(11)
              }

            case (0xd1) => // POP DE
              addTStates(10)
              CHECK_LOG_WORD(SP)
              POP(DE)

            case (0xd2) => // JP NC,nnnn
              JPC(!testFlag(F, FLAG_C))

            case (0xd3) => // OUT (nn),A
              addTStates(11)
              MMU.out8(MMU.get8(PC), A)
              PC.increment()

            case (0xd4) => // CALL NC, nnnn
              CALLC(!testFlag(F, FLAG_C))

            case (0xd5) => // PUSH DE
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(DE)

            case (0xd6) => // SUB nn
              addTStates(7)
              W(MMU.get8(PC))
              PC.increment()
              SUB(W)

            case (0xd7) => // RST 10H
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x0010)

            case (0xd8) => // RET C
              if (testFlag(F, FLAG_C)) {
                CHECK_LOG_WORD(SP)
                POP(PC)
                addTStates(11)
              } else addTStates(5)

            case (0xd9) => // EXX
              addTStates(4)
              var temp = BC.get16
              BC(BCP.get16)
              BCP(temp)
              temp = DE.get16
              DE(DEP.get16)
              DEP(temp)
              temp = HL.get16
              HL(HLP.get16)
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
              ddprefix(op)

            case (0xde) => // SBC A,nn  #4580
              addTStates(7)
              W(MMU.get8(PC))
              PC.increment()
              SBC(A,W)

            case (0xdf) => // RST 18H
              addTStates(18)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x18)

            case (0xe0) => // RET PO
              if (testFlag(F, FLAG_P)) addTStates(5)
              else {
                CHECK_LOG_WORD(SP)
                POP(PC)
                addTStates(11)
              }

            case (0xe1) => // POP HL
              addTStates(10)
              CHECK_LOG_WORD(SP)
              POP(HL)

            case (0xe2) => // JP PO,nnnn
              JPC(!testFlag(F, FLAG_P))

            case (0xe3) => //  EX (SP),HL
              addTStates(19)
              CHECK_LOG_WORD(SP)
              val temp: Int = HL.get16
              POP(HL)
              PUSH(temp)

            case (0xe4) => // CALL PO,nnnn
              CALLC(!testFlag(F, FLAG_P))

            case (0xe5) => // PUSH HL
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(HL)

            case (0xe6) => // AND nn
              addTStates(7)
              AF(andTable(((AF >> 8) & MMU.get8(PC)) & 0xff))
              PC.increment()

            case (0xe7) => // RST 20H
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x20)

            case (0xe8) => // RET PE
              if (testFlag(F, FLAG_P)) {
                CHECK_LOG_WORD(SP)
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
              edprefix(op)


            case (0xee) => // XOR nn
              addTStates(7)
              AF(xororTable(((AF >> 8) ^ MMU.get8(PC)) & 0xff))
              PC.increment()

            case (0xef) => // RST 28H
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x28)

            case (0xf0) => // RET P
              if (testFlag(F, FLAG_S)) {
                addTStates(5)
              } else {
                CHECK_LOG_WORD(SP)
                POP(PC)
                addTStates(11)
              }

            case (0xf1) => // POP AF
              addTStates(10)
              CHECK_LOG_WORD(SP)
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
              CHECK_LOG_WORD(SP - 2)
              PUSH(AF)

            case (0xf6) => // OR nn
              addTStates(7)
              AF(xororTable(((AF >> 8) | MMU.get8(PC)) & 0xff))
              PC.increment()

            case (0xf7) => // RST 30H
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x30)

            case (0xf8) => // RET M
              if (testFlag(F, FLAG_S)) {
                CHECK_LOG_WORD(SP)
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
              fdprefix(op)


            case (0xfe) => // CP nn
              addTStates(7)
              W( MMU.get8(PC))
              PC.increment()
              ICP(W)

            case (0xff) => // RST 38H
              addTStates(11)
              CHECK_LOG_WORD(SP - 2)
              PUSH(PC)
              PC(0x38)

          }


          SimTimer.sim_interval = SimTimer.sim_interval - 1

        } // End Break check
      } // end SwitchCPUNow

      // simulation halted
      onHalt(singleStep | lastBreak != 0)
    } catch {
      case t: Throwable =>
        onHalt(singleStep | lastBreak != 0)
        throw t
    }
  }

  override def onHalt(singleStepped: Boolean): Unit = {
    if (!singleStepped) {
      Utils.outln(s"$getName: Halted.")

      Utils.outln(showRegisters())
      Utils.outln(showFlags())
    } else {
      // Single stepped or break
      val sb = new StringBuilder
      sb.append(f"${PC.intValue}%05X : ")
      DAsm(PC.intValue, sb)
      Utils.outln(sb.toString())

    }
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
  private final def HIGH_DIGIT(x: UByte): Int = {
    (x.intValue >> 4) & 0xf
  }

  @inline
  private final def LOW_DIGIT(x: UByte): Int = {
    x.intValue & 0xf
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
      val t1: UByte = UByte(({
        if (temp == 0) 1 else 0
      } << 6).byteValue())
      val t2: UByte = UByte(({
        if ((temp & 0xf) == 0xf) 1 else 0
      } << 4).byteValue())
      UByte(((temp & 0xa8) | t1 | t2 | 2).toByte)
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
      UShort(((cbits & 0x10) | (((cbits >> 6) ^ (cbits >> 5)) & 4) |
        ((cbits >> 8) & 1) | (cbits & 0xa8)).toShort)
    }
  }.toArray

  private final val cbitsDup16Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte(((cbits & 0x10) | ((cbits >> 8) & 1) | (cbits & 0x28)).toByte)
    }
  }.toArray

  private final val cbits2Table: Array[UByte] = {
    for (cbits <- 0 to 511) yield {
      UByte((cbits & 0x10 | ((cbits >> 8) & 1) | 2).byteValue())
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

  private final val rrdrldTable: Array[UShort] = {
    for (acu <- 0 to 255) yield {
      val t1 = if ((acu & 0xff) == 0) 1 else 0
      UShort(((acu << 8) | (acu & 0xa8) | (t1 << 6) | parityTable(acu)).toShort)
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
  private final def SET_PV(cbits: UInt): Int = ((cbits >> 6) ^ (cbits >> 5)) & 4

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
    val result: Boolean = cond
    if (result) {
      PC(MMU.get16(PC))
    } else PC(PC + 2)
  }

  @inline
  private final def CALLC(cond: => Boolean): Unit = {
    if (cond) {
      val addr = MMU.get16(PC)
      CHECK_LOG_WORD(SP - 2)
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
    val tmp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu + tmp
    AF(addTable(sum & 0x1ff) | cbitsZ80Table((acu ^ tmp ^ sum) & 0x1ff) & 0xffff)
  }

  @inline
  private final def ADD(r1: Register16, r2: Register16): Unit = {
    val sum: UInt = r1.get16 + r2.get16
    if (r1.nmenomic == r2.nmenomic) AF((AF & ~0x3b) | cbitsDup16Table(sum.intValue >> 8))
    else AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((r1 ^ r2 ^ sum) >> 8))
    r1(sum & 0xffff)
  }

  @inline
  private final def ADD(r1: Register8, r2: Register8): Unit = {
    val temp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu + temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0x1ff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADD(r1: Register8, temp: UInt): Unit = {
    val acu: UByte = r1.get8
    val sum: UInt = acu + temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0x1ff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADC(r1: Register8, r2: Register8): Unit = {
    val temp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu + temp + {
      if (testFlag(F, FLAG_C)) UByte(1) else UByte(0)
    }
    val cbits: UInt = acu ^ temp ^ sum
    AF(addTable(sum & 0x1ff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def ADC(r1: Register16, r2: Register16): Unit = {
    val sum: UInt = r1.get16.toUInt + r2.get16.toUInt + {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    if (r1.nmenomic != r2.nmenomic)
      AF((AF & ~0xff) | ((sum >> 8) & 0xa8) | ({
        if ((sum & 0xffff) == 0) UInt(1) else UInt(0)
      } << 6) | cbitsZ80Table(((r1.get16.toUInt ^ r2.get16.toUInt ^ sum) >> 8) & 0x1ff))
    else
      AF((AF & ~0xff) | ({
        if ((sum & 0xffff) == 0) 1 else 0
      } << 6) | cbitsZ80DupTable((sum >> 8) & 0x1ff))

    r1(sum & 0xffff)
  }

  @inline
  private final def ADCIDX(r1: Register8, r2: Register8): Unit = {
    val temp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu + temp + {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    AF(addTable(sum & 0x1ff) | cbitsZ80Table((acu ^ temp ^ sum) & 0x1ff) & 0xffff)
  }

  @inline
  private final def SUB(r1: Register8): Unit = {
    val temp: UByte = r1.get8
    val acu: UByte = A.get8
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SUB(temp: UInt): Unit = {
    val acu: UByte = A.get8
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SUBIDX(r1: Register8): Unit = {
    val temp: UByte = r1.get8
    val acu: UByte = A.get8
    val sum: UInt = acu - temp
    AF((addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)
  }

  @inline
  private final def SBC(r1: Register8, r2: Register8): Unit = {
    val temp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu - temp - {
      if (testFlag(F, FLAG_C)) UByte(1) else UByte(0)
    }
    val cbits: UInt = acu ^ temp ^ sum
    AF(subTable(sum & 0xff) | cbitsTable(cbits & 0x1ff) | SET_PV(cbits))
  }

  @inline
  private final def SBC(r1: Register16, r2: Register16): Unit = {
    val sum: UInt = r1.get16.toUInt - r2.get16.toUInt - (if (testFlag(F, FLAG_C)) UInt(1) else UInt(0))
    if (r1.nmenomic != r2.nmenomic) // Check for HL,HL
      AF((AF & ~0xff) | ((sum >> 8) & 0xa8) | ({
        if ((sum & 0xffff) == 0) UInt(1) else UInt(0)
      } << 6) |
        cbits2Z80Table(((r1.get16.toUInt ^ r2.get16.toUInt ^ sum) >> 8) & 0x1ff))

    else
      AF((AF & ~0xff) | ({
        if ((sum & 0xffff) == 0) UInt(1) else UInt(0)
      } << 6) | cbits2Z80DupTable((sum >> 8) & 0x1ff))

    r1(sum & 0xffff)
  }

  @inline
  private final def SBCAIDX(r1: Register8, r2: Register8): Unit = {
    val temp: UByte = r2.get8
    val acu: UByte = r1.get8
    val sum: UInt = acu - temp - {
      if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
    }
    AF(addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff).intValue)
  }

  @inline
  private final def ICP(r1: Register8): Unit = {
    val temp: UByte = r1.get8
    val acu: UByte = A.get8
    AF((AF & ~0x28) | (temp & 0x28))
    val sum: UInt = acu - temp
    val cbits: UInt = acu ^ temp ^ sum
    AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | SET_PV(cbits) | cbits2Table(cbits & 0x1ff))
  }

  @inline
  private final def LDIDXdd(r1: Register8, ridx: Register16): Unit = {
    val adr: Int = ridx.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    r1(MMU.get8(adr))
  }

  @inline
  private final def LDIDXdd(ridx: Register16, r1: Register8): Unit = {
    val adr = ridx.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
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
    AF((AF & ~0xfe) | decTable(r1.get8.intValue & 0xff) | SET_PV2(0x7f, r1))
  }

  @inline
  private final def INCIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    val temp: UByte = UByte((MMU.get8(adr) + UByte(1)).toByte)
    MMU.put8(adr, temp)
    AF((AF & ~0xfe) | incZ80Table(temp))
  }

  @inline
  private final def DECIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    val temp: UByte = UByte((MMU.get8(adr) - UByte(1)).toByte)
    MMU.put8(adr, temp)
    AF((AF & ~0xfe) | decZ80Table(temp & 0xff))
  }

  @inline
  private final def ANDIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    AF(andTable(((AF >> 8) & MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def XORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    AF(xororTable(((AF >> 8) ^ MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def ORIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    AF(xororTable(((AF >> 8) | MMU.get8(adr)) & 0xff))
  }

  @inline
  private final def CPIX8(r1: Register8): Unit = {
    val temp: UByte = r1.get8
    AF((AF & ~0x28) | (temp & 0x28))
    val acu: UByte = A.get8
    val sum: UInt = acu - temp
    AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))
  }

  @inline
  private final def CPIDXdd(r1: Register16): Unit = {
    val adr: Int = r1.get16.intValue + MMU.get8(PC)
    PC.increment()
    CHECK_LOG_BYTE(adr)
    val temp: UByte = MMU.get8(adr)
    AF((AF & ~0x28) | (temp & 0x28))
    val acu: UByte = A.get8
    val sum: UInt = acu - temp
    AF((AF & ~0xff) | cpTable(sum & 0xff) | (temp & 0x28) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff))
  }

  @inline
  private final def NEG: Unit = {
    addTStates(8)
    val temp: UByte = A.get8
    AF((~(AF & 0xff00) + 1) & 0xff00)
    AF(AF | ((AF >> 8) & 0xa8) | ({
      if ((AF & 0xff00) == 0) 1 else 0
    } << 6) | negTable(temp))

  }

  @inline
  private final def RETN: Unit = {
    addTStates(14)
    IFF(IFF | (IFF >> 1))
    CHECK_LOG_WORD(SP)
    POP(PC)
  }

  @inline
  private final def setFlag(flag: Int, clear: Boolean): Unit = {
    AF(if (clear) (AF | flag) else (AF & ~flag))

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
    INOUTFLAGS((C.get8 & 0xa8) | ({
      if (C.get8 == 0) 1 else 0
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


  override def DAsm(addr: Int, sb: StringBuilder): Int = {

    var pc = addr

    var T: String = null
    var J = false
    var Offset = 0

    var C: Char = 0x00

    val op = MMU.get8(pc).intValue

    op match {

      case (0xcb) =>
        pc += 1
        T = Z80.MnemonicsCB(MMU.get8(pc).intValue)
        pc += 1

      case (0xed) =>
        pc += 1
        T = Z80.MnemonicsED(MMU.get8(pc).intValue)

      case (0xdd) | (0xfd) =>

        C = {
          val x = MMU.get8(pc).intValue
          pc += 1
          if (x == 0xdd) 'X' else 'Y'

        }
        if (MMU.get8(pc).intValue == 0xcb) {
          pc += 1
          Offset = MMU.get8(pc)
          pc += 1
          J = true
          T = Z80.MnemonicsXCB(MMU.get8(pc).intValue)
          pc += 1
        }
        else {
          T = Z80.MnemonicsXX(MMU.get8(pc).intValue)
          pc += 1
        }

      case _ =>
        T = Z80.MnemonicsZ80(MMU.get8(pc).intValue)
        pc += 1
    }

    var R: String = if (T.contains("^")) {
      val x = MMU.get8(pc).intValue
      pc += 1
      T.replace("^", f"$x%02x")


    } else T

    R = R.replace('%', C)

    R = if (R.contains("*")) {
      val x = MMU.get8(pc).intValue
      pc += 1
      R.replace("*", f"$x%02x")
    } else if (R.contains("@")) {
      if (J) {
        Offset = MMU.get8(pc).intValue
        pc += 1

      }
      val S = {
        if ((Offset & 0x80) != 0) "-" else "+"
      }
      val j = {
        if ((Offset & 0x80) != 0) 256 - Offset else Offset
      }
      R.replace("@", f"$S$j%02x")
    } else if (R.contains("$")) {
      Offset = MMU.get8(pc).intValue
      pc += 1
      val x = pc + 2 + {
        if ((Offset & 0x80) != 0) Offset - 256 else Offset
      } & 0xFFFF
      pc += 2
      R.replace("$", f"$x%04x")

    } else if (R.contains("#")) {
      val x = MMU.get8(pc).intValue + 256 * MMU.get8(pc + 1).intValue
      pc += 2
      R.replace("#", f"$x%04x")

    } else R

    sb.append(R)
    pc
  }

  /**
    * CB Prefix
    * @param op
    * @param adr
    */
  private def cbprefix(op: Int, adr: Int): Unit = {
    var acu: UByte = UByte(0)
    var cbits: UInt = UInt(0)
    var temp: UByte = UByte(0)
    var tStateModifier = false


    (op & 7) match {
      case (0) =>
        PC.increment()
        acu = B.get8
        addTStates(8)
      case (1) =>
        PC.increment()
        acu = C.get8
        addTStates(8)
      case (2) =>
        PC.increment()
        acu = D.get8
        addTStates(8)
      case (3) =>
        PC.increment()
        acu = E.get8
        addTStates(8)
      case (4) =>
        PC.increment()
        acu = H.get8
      case (5) =>
        PC.increment()
        acu = L.get8
        addTStates(8)
      case (6) =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)
        addTStates(15)
        tStateModifier = true
      case (7) =>
        PC.increment()
        acu = A.get8
        addTStates(8)
      case _ =>

    }
    (op & 0xc0) match {
      case (0x00) => // shift/rotate
        (op & 0x38) match {
          case (0x00) => //RLC
            temp = UByte(((acu << 1) | (acu >> 7)).byteValue)
            cbits = UInt(temp & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x08) => // RRC
            temp = UByte(((acu >> 1) | (acu << 7)).byteValue)
            cbits = UInt(temp & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x10) => // RL
            temp = UByte(((acu >> 1) | {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }).byteValue)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x18) => // RR
            temp = UByte(((acu >> 1) | ({
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            } << 7)).byteValue)
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x20) => // SLA
            temp = UByte((acu << 1).byteValue)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x28) => // SRA
            temp = UByte(((acu >> 1) | (acu & 0x80)).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x30) => // SLIA
            temp = UByte(((acu << 1) | UByte(1)).byteValue)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp & 0xff) | {
              if (cbits == 0) 0 else 1
            })
          case (0x38) => // SRL
            temp = UByte((acu >> 1).byteValue)
            cbits = UInt(acu & 1)
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
        temp = UByte((acu & ~(1 << ((op >> 3) & 7))).byteValue())
      case (0xc0) => // SET
        temp = UByte((acu | (1 << ((op >> 3) & 7))).byteValue())
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
        MMU.put8(adr, temp)
      case (7) =>
        A(temp)
      case _ =>
    }
  }

  /**
    * DD Prefix
    *
    */
  private def ddprefix(op: Int): Unit = {
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
        CHECK_LOG_WORD(temp)
        MMU.put16(temp, IX.get16)
        PC(PC + 2)

      case (0x23) => // INC IX
        addTStates(10)
        IX.increment()

      case (0x24) => // INC IXH
        addTStates(9)
        IXH.increment()
        AF((AF & ~0xfe) | incZ80Table(IXH.get8))

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
        CHECK_LOG_WORD(tmp)
        IX(MMU.get16(tmp))
        PC(PC + 2)

      case (0x2b) => // DEC IX
        addTStates(9)
        IX.decrement()

      case (0x2c) => // INC IXL
        addTStates(9)
        IXL.increment()
        AF((AF & ~0xfe) | incZ80Table(IXL.get8))

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
        CHECK_LOG_BYTE(adr)
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
        val adr: Int = IX.get16 + MMU.get8(PC).intValue
        PC.increment()
        CHECK_LOG_BYTE(adr)
        W(MMU.get8(adr))
        ADDIDX(A,W)

      case (0x8c) => // ADC A,IXH
        addTStates(9)
        ADCIDX(A, IXH)

      case (0x8d) => // ADC A,IXL
        addTStates(9)
        ADCIDX(A, IXL)

      case (0x8e) => // ADC A,(IX+dd)
        addTStates(19)
        val adr: Int = IX + MMU.get8(PC)
        CHECK_LOG_BYTE(adr)
        W(MMU.get8(adr))
        ADCIDX(A,W)

      case (0x96) => // SUB (IX+dd)
        addTStates(19)
        val adr: Int = IX + MMU.get8(PC)
        CHECK_LOG_BYTE(adr)
        val temp: UByte = MMU.get8(adr)
        val acu: UByte = A.get8
        val sum: UInt = acu - temp
        AF((addTable(sum & 0xff) | cbits2Z80Table((acu ^ temp ^ sum) & 0x1ff)).intValue)

      case (0x94) => // SUB IXH
        addTStates(9)
        setFlag(FLAG_C, clear = false)
        SUBIDX(IXH)

      case (0x9c) => // SBC A,IXH
        addTStates(9)
        SBCAIDX(A, IXH)

      case (0x95) => // SUB IXL
        addTStates(9)
        setFlag(FLAG_C, clear = false)
        SUBIDX(IXL)

      case (0x9d) => // SBC A,IXL
        addTStates(9)
        SBCAIDX(A, IXL)

      case (0x9e) => // SBC A,(IX+dd)
        addTStates(19)
        val adr: Int = IX + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr)
        W(MMU.get8(adr))
        SBCAIDX(A,W)

      case (0xa4) => // AND IXH
        addTStates(9)
        AF(andTable((AF.get16 & IX.get16) >> 8 & 0xff))

      case (0xa5) => // AND IXL
        addTStates(9)
        AF(andTable(((AF.get16 >> 8) & IX.get16) & 0xff))

      case (0xa6) => // AND (IX+dd)
        addTStates(19)
        ANDIDXdd(IX)

      case (0xac) => // XOR IXH
        addTStates(9)
        AF(xororTable(((AF.get16 ^ IX.get16) >> 8) & 0xff))

      case (0xad) => // XOR IXL
        addTStates(9)
        AF(xororTable(((AF.get16 >> 8) ^ IX.get16) & 0xff))

      case (0xae) => // XOR (IX+DD)
        addTStates(19)
        XORIDXdd(IX)

      case (0xb4) => // OR IXH
        addTStates(9)
        AF(xororTable(((AF.get16 | IX.get16) >> 8) & 0xff))

      case (0xb5) => // OR IXL
        addTStates(9)
        AF(xororTable(((AF.get16 >> 8) | IX.get16) & 0xff))

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
      case (0xcb) =>
        //  DD CB PREFIX
        val adr: Int = IX.get16.intValue + MMU.get8(PC)
        PC.increment()
        INCR(1)
        val op: Int = MMU.get8(PC)
        ddcbprefix(op, adr)

      case (0xe1) => // POP IX
        addTStates(14)
        CHECK_LOG_WORD(SP)
        POP(IX)

      case (0xe3) => // EX (SP),IX
        addTStates(23)
        CHECK_LOG_WORD(SP)
        val tmp: Int = IX.get16
        POP(IX)
        PUSH(tmp)

      case (0xe5) => // PUSH IX
        addTStates(15)
        CHECK_LOG_WORD(SP - 2)
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
  }

  /**
    * ED Prefix
    *
    * @param op
    */
  private def edprefix(op: Int): Unit = {
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
        CHECK_LOG_WORD(temp)
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
        CHECK_LOG_WORD(temp)
        BC(MMU.get16(temp))
        PC(PC + 2)

      case (0x4d) => // RETI
        addTStates(14)
        IFF(IFF | IFF >> 1)
        CHECK_LOG_WORD(SP)
        POP(PC)

      case (0x4f) => // LD R,A
        addTStates(9)
        IR((IR & ~0xff) | ((AF >> 8) & 0xff))

      case (0x50) => // IN D,(C)
        addTStates(12)
        val temp: UByte = MMU.in8(C)
        D(temp)
        AF((AF & 0xfe) | rotateShiftTable(temp))

      case (0x51) => // OUT (C),D
        addTStates(12)
        MMU.out8(C, D)

      case (0x52) => // SBC HL,DE
        addTStates(15)
        SBC(HL, DE)

      case (0x53) => // LD (nnnn),DE
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
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
        val temp: UByte = MMU.in8((C))
        E(temp)
        AF((AF & 0xfe) | rotateShiftTable(temp))

      case (0x59) => // OUT (C),E
        addTStates(12)
        MMU.out8(C, E)

      case (0x5a) => // ADC HL,DE
        addTStates(15)
        ADC(HL, DE)

      case (0x5b) => // LD DE,(nnnn)
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
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
        val temp: UByte = MMU.in8(C)
        H(temp)
        AF((AF & 0xfe) | rotateShiftTable(temp))

      case (0x61) => // OUT (C),H
        addTStates(12)
        MMU.out8(C, H)

      case (0x62) => // SBC HL,HL
        addTStates(15)
        SBC(HL, HL)

      case (0x63) => // LD (nnnn),HL
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
        MMU.put16(temp, HL)
        PC(PC + 2)

      case (0x67) => // RRD
        addTStates(18)
        val temp: UByte = MMU.get8(HL)
        val acu: UByte = A.get8
        MMU.put8(HL, HIGH_DIGIT(temp) | (LOW_DIGIT(acu) << 4) & 0xff)
        AF(rrdrldTable((acu & 0xf0) | LOW_DIGIT(temp)) | (AF.get16 & 1))

      case (0x68) => // IN L,(C)
        addTStates(12)
        val temp: UByte = MMU.in8(C)
        L(temp)
        AF((AF & 0xfe) | rotateShiftTable(temp & 0xff))

      case (0x69) => // OUT (C),L
        addTStates(12)
        MMU.out8(C, L)

      case (0x6a) => // ADC HL,HL
        addTStates(15)
        ADC(HL, HL)

      case (0x6b) => // LD HL,(nnnn)
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
        HL(MMU.get16(temp))
        PC(PC + 2)

      case (0x6f) => // RLD
        addTStates(18)
        val temp: UByte = MMU.get8(HL)
        val acu: UByte = A.get8
        MMU.put8(HL, (LOW_DIGIT(temp) << 4) | LOW_DIGIT(acu))
        AF(rrdrldTable((acu & 0xf0) | HIGH_DIGIT(temp)) | (F.get8.intValue & 1))

      case (0x70) => // IN (C)
        addTStates(12)
        val temp: UByte = MMU.in8(C)
        AF((AF & 0xfe) | rotateShiftTable(temp))

      case (0x71) => // OUT (C),0
        addTStates(12)
        MMU.out8(C, UByte(0))

      case (0x72) => // SBC HL,SP
        addTStates(15)
        SBC(HL, SP)

      case (0x73) => // LD (nnnn),SP
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
        MMU.put16(temp, SP)
        PC(PC + 2)

      case (0x78) => // IN A,(C)
        addTStates(12)
        val temp: UByte = MMU.in8(C)
        A(temp)
        AF((AF & 0xfe) | rotateShiftTable(temp))

      case (0x79) => // OUT (C),A
        addTStates(12)
        MMU.out8(C, A)

      case (0x7a) => // ADC HL,SP
        addTStates(15)
        ADC(HL, SP)

      case (0x7b) => // LD SP,(nnnn)
        addTStates(20)
        val temp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(temp)
        SP(MMU.get16(temp))
        PC(PC + 2)

      case (0xa0) => // LDI
        addTStates(16)
        // CHECK_BREAK_TWO_BYTES - HL & DE
        var acu: UInt = MMU.get8(HL)
        HL.increment()
        MMU.put8(DE, acu.intValue)
        DE.increment()
        acu = acu + A.get8
        AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | ({
          BC.decrement()
          if ((BC & 0xffff) != 0) 1 else 0
        } << 2))

      case (0xa1) => // CPI
        addTStates(16)
        CHECK_LOG_BYTE(HL)
        val acu: UByte = A.get8
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
        CHECK_LOG_BYTE(HL)
        val acu: UInt = MMU.in8(C)
        MMU.put8(HL, acu.intValue)
        HL.increment()
        val temp: UByte = B.get8
        BC(BC - 0x100)
        INOUTFLAGS_NONZERO((C + 1) & 0xff, acu.intValue, temp.intValue)

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
        CHECK_LOG_BYTE(HL)
        val acu: UInt = MMU.get8(HL)
        MMU.out8(C, UByte(acu.byteValue()))
        HL.increment()
        val temp: UByte = B.get8
        BC(BC - 0x100)
        INOUTFLAGS_NONZERO(L.get8, acu.intValue, temp.intValue)

      case (0xa8) => // LDD
        addTStates(16)
        // CHECK_BREAK_TWO_BYTES HL,DE
        var acu: UInt = MMU.get8(HL)
        HL.decrement()
        MMU.put8(DE, acu.intValue)
        DE.decrement()
        acu = acu + A.get8
        AF((AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) | {
          BC.decrement()
          if ((BC & 0xffff) != 0) 1 else 0
        } << 2)

      case (0xa9) => // CPD
        addTStates(16)
        CHECK_LOG_BYTE(HL)
        val acu: UByte = A.get8
        val temp: UByte = MMU.get8(HL)
        HL.decrement()
        val sum: UInt = acu - temp
        val cbits: UInt = acu ^ temp ^ sum
        AF(
          (AF & ~0xfe) | (sum & 0x80) | ({
            if ((sum & 0xff) == 0) UInt(1) else UInt(0)
          } << 6) |
            (((sum - ((cbits & 16) >> 4)) & 2) << 4) | (cbits & 16) |
            ((sum - ((cbits >> 4) & 1)) & 8) | {
            BC.decrement()
            if ((BC & 0xffff) != 0) UInt(1) else UInt(0)
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
        CHECK_LOG_BYTE(HL)
        val acu: UInt = MMU.in8(C)
        MMU.put8(HL, acu.intValue)
        HL.decrement()
        val temp: Int = B.get8
        BC(BC.get16 - 0x100)
        INOUTFLAGS_NONZERO((C.get8 - 1) & 0xff, acu.intValue, temp)

      case (0xab) => // OUTD
        addTStates(16)
        CHECK_LOG_BYTE(HL)
        val acu: UInt = MMU.get8(HL)
        MMU.out8(C, acu.intValue)
        HL.decrement()
        val temp: Int = B.get8
        BC(BC.get16 - 0x100)
        INOUTFLAGS_NONZERO(L.get8, acu.intValue, temp.intValue())

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
        val acu: UByte = A.get8
        var bc: UInt = BC.get16
        var temp: UInt = UInt(0)
        var sum: UInt = UInt(0)
        var op: Int = 0

        if (bc == 0) bc = UInt(0x10000)
        do {
          addTStates(21)
          INCR(1)
          CHECK_LOG_BYTE(HL)
          temp = MMU.get8(HL)
          HL.increment()
          bc -= UInt(1)
          op = {
            if (bc != 0) 1 else 0
          }
          sum = acu - temp

        } while (bc != 0 && sum != 0)
        val cbits :UInt  = acu ^ temp ^ sum
        BC(0)
        val nc = {
          if ((sum & 0xff) == 0) UInt(1) else UInt(0)
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
          CHECK_LOG_BYTE(HL)
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
          CHECK_LOG_BYTE(HL)
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
        val acu: UByte = A.get8
        var bc: UInt = BC.get16
        var temp: UByte = UByte(0)
        var sum: UInt = UInt(0)
        var op: Int = 0

        if (bc == 0) bc = UInt(0x10000)
        do {
          addTStates(21)
          INCR(1)
          CHECK_LOG_BYTE(HL)
          temp = MMU.get8(HL)
          HL.decrement()
          bc -= UInt(1)
          op = {
            if (bc != 0) 1 else 0
          }
          sum = acu - temp

        } while (bc != 0 && sum != 0)
        val cbits: UInt = acu ^ temp ^ sum
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
          if (B.intValue == 0) 0x100 else B.get8
        }
        var acu: Int = 0
        do {
          addTStates(21)
          INCR(1)
          CHECK_LOG_BYTE(HL)
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
          CHECK_LOG_BYTE(HL)
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
  }

  /**
    * FD Prefix
    */
  private def fdprefix(op: Int): Unit = {

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
        CHECK_LOG_WORD(temp)
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
        val sum: UInt = IY.get16 + IY.get16
        AF((AF & ~0x3b) | cbitsDup16Table(sum.intValue >> 8))
        IY(sum & 0xffff)

      case (0x2a) => // LD IY,(nnnn)
        addTStates(20)
        val tmp: Int = MMU.get16(PC)
        CHECK_LOG_WORD(tmp)
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
        addTStates(9)
        IYL(MMU.get8(PC))
        PC.increment()

      case (0x34) => // INC (IY+dd)
        addTStates(23)
        INCIDXdd(IY)

      case (0x35) => // DEC (IY+dd)
        addTStates(23)
        DECIDXdd(IY)

      case (0x36) => // LD (IY+dd),nn
        addTStates(19)
        val adr: Int = IY.get16.intValue + MMU.get8(PC).intValue
        PC.increment()
        CHECK_LOG_BYTE(adr)
        MMU.put8(adr, MMU.get8(PC))
        PC.increment()

      case (0x39) => // ADD IY,SP
        addTStates(15)
        val sum: UInt = IY.get16 + SP.get16
        AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((IY ^ SP ^ sum) >> 8))
        IY(sum & 0xffff)

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
        ADDIDX(A, IYH)

      case (0x85) => // ADD A,IYL
        addTStates(9)
        ADDIDX(A, IYL)


      case (0x86) => // ADD A,(IY+dd)
        addTStates(19)
        val adr: UInt = IY.get16 + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr.intValue)
        W(MMU.get8(adr.intValue))
        ADDIDX(A,W)


      case (0x8c) => // ADC A,IYH
        addTStates(9)
        ADCIDX(A, IYH)

      case (0x8d) => // ADC A,IYL
        addTStates(9)
        ADCIDX(A, IYL)

      case (0x8e) => // ADC A,(IY+dd)
        addTStates(19)
        val adr: UInt = IY.get16 + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr.intValue)
        W(MMU.get8(adr.intValue))
        ADCIDX(A,W)

      case (0x96) => // SUB (IY+dd)
        addTStates(19)
        val adr: UInt = IY.get16 + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr.intValue)
        W(MMU.get8(adr.intValue))
        SUBIDX(W)

      case (0x94) => // SUB IYH
        addTStates(9)
        setFlag(FLAG_C, clear = false)
        SUBIDX(IYH)

      case (0x9c) => // SBC A,IYH
        addTStates(9)
        SBCAIDX(A, IYH)

      case (0x95) => // SUB IYL
        addTStates(9)
        setFlag(FLAG_C, clear = false)
        SUBIDX(IYL)

      case (0x9d) => // SBC A,IYL
        addTStates(9)
        SBCAIDX(A, IYL)

      case (0x9e) => // SBC A,(IY+dd)
        addTStates(19)
        val adr: UInt = IY.get16 + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr.intValue)
        W(MMU.get8(adr.intValue))
        SBCAIDX(A,W)

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
        CHECK_LOG_BYTE(adr)
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
        CHECK_LOG_BYTE(adr)
        AF(xororTable(((AF >> 8) ^ MMU.get8(adr)) & 0xff))

      case (0xb4) => // OR IYH
        addTStates(9)
        AF(xororTable(((AF | IY) >> 8) & 0xff))

      case (0xb5) => // OR IYL
        addTStates(9)
        AF(xororTable(((AF >> 8) | IY) & 0xff))

      case (0xbc) => // CP IYH
        addTStates(9)
        W(IYH.get8)
        ICP(W)

      case (0xbd) => // CP IYL
        addTStates(9)
        W(IYL.get8)
        ICP(W)

      case (0xbe) => // CP (IY+dd)
        addTStates(9)
        val adr: UInt = IY.get16 + MMU.get8(PC)
        PC.increment()
        CHECK_LOG_BYTE(adr.intValue)
        W(MMU.get8(adr.intValue))
        ICP(W)


      case (0xcb) => // ******************************************************** FD CB Prefix
        val adr: Int = IY + MMU.get8(PC)
        PC.increment()
        val op: Int = MMU.get8(PC)
        fdcbprefix(op, adr)

      case (0xe1) => // POP IY
        addTStates(14)
        CHECK_LOG_WORD(SP)
        POP(IY)

      case (0xe3) => // EX (SP),IY
        addTStates(23)
        CHECK_LOG_WORD(SP)
        val tmp: UInt = IY.get16
        POP(IY)
        PUSH(tmp.intValue)

      case (0xe5) => // PUSH IY
        addTStates(15)
        CHECK_LOG_WORD(SP - 2)
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
  }

  /**
    * DD CB Prefix
    */
  private def ddcbprefix(op: Int, adr: Int): Unit = {

    var acu: UByte = UByte(0)
    var temp: UByte = UByte(0)
    var cbits: UInt = UInt(0)

    (op & 7) match {
      case 0 =>
        PC.increment()
        acu = B.get8

      case 1 =>
        PC.increment()
        acu = C.get8

      case 2 =>
        PC.increment()
        acu = D.get8

      case 3 =>
        PC.increment()
        acu = E.get8

      case 4 =>
        PC.increment()
        acu = H.get8

      case 5 =>
        PC.increment()
        acu = L.get8

      case 6 =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)

      case 7 =>
        PC.increment()
        acu = A.get8

      case _ =>

    }
    (op & 0xc0) match {
      case (0x00) => {
        // Shift/rotate
        // #4438
        addTStates(23)
        (op & 0x38) match {
          case (0x00) => // RLC
            temp = UByte(((acu << 1) | (acu >> 7)).byteValue)
            cbits = UInt(temp & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x08) => // RRC
            temp = UByte(((acu >> 1) | (acu << 7)).byteValue)
            cbits = UInt(temp & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x10) => // RL
            temp = UByte(((acu >> 1) | {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }).byteValue())
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x18) => // RR
            temp = UByte(((acu >> 1) | ({
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            } << 7)).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x20) => // SLA
            temp = UByte((acu << 1).byteValue)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x28) => // SRA
            temp = UByte(((acu >> 1) | (acu & 0x80)).byteValue())
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x30) => // SLIA
            temp = UByte(((acu << 1) | 1).byteValue)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x38) => // SRL
            temp = UByte((acu >> 1).byteValue)
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(temp) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case _ =>
        }
      }


      case (0x40) => // BIT
        addTStates(20)
        if ((acu & (1 << ((op >> 3) & 7))) != 0) AF((AF & ~0xfe) | 0x10 | ({
          if ((op & 0x38) == 0x38) UInt(1) else UInt(0)
        } << 7))
        else AF((AF & ~0xfe) | 0x54)
        if ((op & 7) != 6) AF(AF | (acu & 0x28))
        temp = acu

      case (0x80) => // RES
        addTStates(23)
        temp = UByte((acu & ~(1 << ((op >> 3) & 7))).byteValue())

      case (0xc0) => // SET
        addTStates(23)
        temp = UByte((acu | (1 << ((op >> 3) & 7))).byteValue())

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
        MMU.put8(adr, temp)

      case (7) =>
        A(temp)

      case _ =>
    }

  }

  /**
    * FD CB Prefix ops
    */
  private def fdcbprefix(op: Int, adr: Int): Unit = {

    var acu: UByte = UByte(0)
    var cbits: UInt = UInt(0)
    var tmp: UInt = UInt(0)

    (op & 7) match {
      case 0 =>
        PC.increment()
        acu = B.get8
      case 1 =>
        PC.increment()
        acu = C.get8
      case 2 =>
        PC.increment()
        acu = D.get8
      case 3 =>
        PC.increment()
        acu = E.get8
      case 4 =>
        PC.increment()
        acu = H.get8
      case 5 =>
        PC.increment()
        acu = L.get8
      case 6 =>
        CHECK_LOG_BYTE(adr)
        PC.increment()
        acu = MMU.get8(adr)
      case 7 =>
        PC.increment()
        acu = A.get8
      case _ =>
    }
    (op & 0xc0) match {

      case (0x00) => // shift/rotate
        (op & 0x38) match {
          case (0x00) => // RLC
            tmp = UInt((acu.intValue << 1) | (acu.intValue >> 7))
            cbits = UInt(tmp & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x08) => // RRC
            tmp = (acu >> 1) | (acu << 7)
            cbits = UInt(tmp & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x10) => // RL
            tmp = (acu << 1) | {
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            }
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x18) => // RR
            tmp = (acu >> 1) | ({
              if (testFlag(F, FLAG_C)) UInt(1) else UInt(0)
            } << 7)
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x20) => // SLA
            tmp = (acu << 1)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) UInt(0) else UInt(1)
            })

          case (0x28) => // SRA
            tmp = UInt((acu >> 1) | (acu & 0x80))
            cbits = UInt(acu & 1)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x30) => // SLIA
            tmp = (acu << 1) | UInt(1)
            cbits = UInt(acu & 0x80)
            AF((AF & ~0xff) | rotateShiftTable(tmp & 0xff) | {
              if (cbits == 0) 0 else 1
            })

          case (0x38) => // SRL
            tmp = acu >> 1
            cbits = UInt(acu & 1)
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
        tmp = UInt(acu & ~(1 << ((op >> 3) & 7)))

      case (0xc0) => // SET
        addTStates(23)
        tmp = UInt(acu | (1 << ((op >> 3) & 7)))

      case _ =>
    }
    (op & 7) match {
      case 0 =>
        B(tmp & 0xff)
      case 1 =>
        C(tmp & 0xff)
      case 2 =>
        D(tmp & 0xff)
      case 3 =>
        E(tmp & 0xff)
      case 4 =>
        H(tmp & 0xff)
      case 5 =>
        L(tmp & 0xff)
      case 6 =>
        MMU.put8(adr, UByte(tmp.byteValue()))

      case 7 =>
        A(tmp & 0xff)

      case _ =>
    }

  }
}


object Z80 {

  final val MnemonicsZ80: List[String] =
    List("NOP", "LD BC,#h", "LD (BC),A", "INC BC", "INC B", "DEC B", "LD B,*h", "RLCA", /*  00-07   */
      "EX AF,AF'", "ADD HL,BC", "LD A,(BC)", "DEC BC", "INC C", "DEC C", "LD C,*h", "RRCA", /*  08-0f   */
      "DJNZ $h", "LD DE,#h", "LD (DE),A", "INC DE", "INC D", "DEC D", "LD D,*h", "RLA", /*  10-17   */
      "JR $h", "ADD HL,DE", "LD A,(DE)", "DEC DE", "INC E", "DEC E", "LD E,*h", "RRA", /*  18-1f   */
      "JR NZ,$h", "LD HL,#h", "LD (#h),HL", "INC HL", "INC H", "DEC H", "LD H,*h", "DAA", /*  20-27   */
      "JR Z,$h", "ADD HL,HL", "LD HL,(#h)", "DEC HL", "INC L", "DEC L", "LD L,*h", "CPL", /*  28-2f   */
      "JR NC,$h", "LD SP,#h", "LD (#h),A", "INC SP", "INC (HL)", "DEC (HL)", "LD (HL),*h", "SCF", /*  30-37   */
      "JR C,$h", "ADD HL,SP", "LD A,(#h)", "DEC SP", "INC A", "DEC A", "LD A,*h", "CCF", /*  38-3f   */
      "LD B,B", "LD B,C", "LD B,D", "LD B,E", "LD B,H", "LD B,L", "LD B,(HL)", "LD B,A", /*  40-47   */
      "LD C,B", "LD C,C", "LD C,D", "LD C,E", "LD C,H", "LD C,L", "LD C,(HL)", "LD C,A", /*  48-4f   */
      "LD D,B", "LD D,C", "LD D,D", "LD D,E", "LD D,H", "LD D,L", "LD D,(HL)", "LD D,A", /*  50-57   */
      "LD E,B", "LD E,C", "LD E,D", "LD E,E", "LD E,H", "LD E,L", "LD E,(HL)", "LD E,A", /*  58-5f   */
      "LD H,B", "LD H,C", "LD H,D", "LD H,E", "LD H,H", "LD H,L", "LD H,(HL)", "LD H,A", /*  60-67   */
      "LD L,B", "LD L,C", "LD L,D", "LD L,E", "LD L,H", "LD L,L", "LD L,(HL)", "LD L,A", /*  68-6f   */
      "LD (HL),B", "LD (HL),C", "LD (HL),D", "LD (HL),E", "LD (HL),H", "LD (HL),L", "HALT", "LD (HL),A", /*  70-77   */
      "LD A,B", "LD A,C", "LD A,D", "LD A,E", "LD A,H", "LD A,L", "LD A,(HL)", "LD A,A", /*  78-7f   */
      "ADD A,B", "ADD A,C", "ADD A,D", "ADD A,E", "ADD A,H", "ADD A,L", "ADD A,(HL)", "ADD A,A", /*  80-87   */
      "ADC A,B", "ADC A,C", "ADC A,D", "ADC A,E", "ADC A,H", "ADC A,L", "ADC A,(HL)", "ADC A,A", /*  88-8f   */
      "SUB B", "SUB C", "SUB D", "SUB E", "SUB H", "SUB L", "SUB (HL)", "SUB A", /*  90-97   */
      "SBC A,B", "SBC A,C", "SBC A,D", "SBC A,E", "SBC A,H", "SBC A,L", "SBC A,(HL)", "SBC A,A", /*  98-9f   */
      "AND B", "AND C", "AND D", "AND E", "AND H", "AND L", "AND (HL)", "AND A", /*  a0-a7   */
      "XOR B", "XOR C", "XOR D", "XOR E", "XOR H", "XOR L", "XOR (HL)", "XOR A", /*  a8-af   */
      "OR B", "OR C", "OR D", "OR E", "OR H", "OR L", "OR (HL)", "OR A", /*  b0-b7   */
      "CP B", "CP C", "CP D", "CP E", "CP H", "CP L", "CP (HL)", "CP A", /*  b8-bf   */
      "RET NZ", "POP BC", "JP NZ,#h", "JP #h", "CALL NZ,#h", "PUSH BC", "ADD A,*h", "RST 00h", /*  c0-c7   */
      "RET Z", "RET", "JP Z,#h", "PFX_CB", "CALL Z,#h", "CALL #h", "ADC A,*h", "RST 08h", /*  c8-cf   */
      "RET NC", "POP DE", "JP NC,#h", "OUT (*h),A", "CALL NC,#h", "PUSH DE", "SUB *h", "RST 10h", /*  d0-d7   */
      "RET C", "EXX", "JP C,#h", "IN A,(*h)", "CALL C,#h", "PFX_DD", "SBC A,*h", "RST 18h", /*  d8-df   */
      "RET PO", "POP HL", "JP PO,#h", "EX (SP),HL", "CALL PO,#h", "PUSH HL", "AND *h", "RST 20h", /*  e0-e7   */
      "RET PE", "LD PC,HL", "JP PE,#h", "EX DE,HL", "CALL PE,#h", "PFX_ED", "XOR *h", "RST 28h", /*  e8-ef   */
      "RET P", "POP AF", "JP P,#h", "DI", "CALL P,#h", "PUSH AF", "OR *h", "RST 30h", /*  f0-f7   */
      "RET M", "LD SP,HL", "JP M,#h", "EI", "CALL M,#h", "PFX_FD", "CP *h", "RST 38h") /*  f8-ff   */


  final val MnemonicsCB: List[String] =
    List("RLC B", "RLC C", "RLC D", "RLC E", "RLC H", "RLC L", "RLC (HL)", "RLC A", /*  00-07   */
      "RRC B", "RRC C", "RRC D", "RRC E", "RRC H", "RRC L", "RRC (HL)", "RRC A", /*  08-0f   */
      "RL B", "RL C", "RL D", "RL E", "RL H", "RL L", "RL (HL)", "RL A", /*  10-17   */
      "RR B", "RR C", "RR D", "RR E", "RR H", "RR L", "RR (HL)", "RR A", /*  18-1f   */
      "SLA B", "SLA C", "SLA D", "SLA E", "SLA H", "SLA L", "SLA (HL)", "SLA A", /*  20-27   */
      "SRA B", "SRA C", "SRA D", "SRA E", "SRA H", "SRA L", "SRA (HL)", "SRA A", /*  28-2f   */
      "SLL B", "SLL C", "SLL D", "SLL E", "SLL H", "SLL L", "SLL (HL)", "SLL A", /*  30-37   */
      "SRL B", "SRL C", "SRL D", "SRL E", "SRL H", "SRL L", "SRL (HL)", "SRL A", /*  38-3f   */
      "BIT 0,B", "BIT 0,C", "BIT 0,D", "BIT 0,E", "BIT 0,H", "BIT 0,L", "BIT 0,(HL)", "BIT 0,A", /*  40-47   */
      "BIT 1,B", "BIT 1,C", "BIT 1,D", "BIT 1,E", "BIT 1,H", "BIT 1,L", "BIT 1,(HL)", "BIT 1,A", /*  48-4f   */
      "BIT 2,B", "BIT 2,C", "BIT 2,D", "BIT 2,E", "BIT 2,H", "BIT 2,L", "BIT 2,(HL)", "BIT 2,A", /*  50-57   */
      "BIT 3,B", "BIT 3,C", "BIT 3,D", "BIT 3,E", "BIT 3,H", "BIT 3,L", "BIT 3,(HL)", "BIT 3,A", /*  58-5f   */
      "BIT 4,B", "BIT 4,C", "BIT 4,D", "BIT 4,E", "BIT 4,H", "BIT 4,L", "BIT 4,(HL)", "BIT 4,A", /*  60-67   */
      "BIT 5,B", "BIT 5,C", "BIT 5,D", "BIT 5,E", "BIT 5,H", "BIT 5,L", "BIT 5,(HL)", "BIT 5,A", /*  68-6f   */
      "BIT 6,B", "BIT 6,C", "BIT 6,D", "BIT 6,E", "BIT 6,H", "BIT 6,L", "BIT 6,(HL)", "BIT 6,A", /*  70-77   */
      "BIT 7,B", "BIT 7,C", "BIT 7,D", "BIT 7,E", "BIT 7,H", "BIT 7,L", "BIT 7,(HL)", "BIT 7,A", /*  78-7f   */
      "RES 0,B", "RES 0,C", "RES 0,D", "RES 0,E", "RES 0,H", "RES 0,L", "RES 0,(HL)", "RES 0,A", /*  80-87   */
      "RES 1,B", "RES 1,C", "RES 1,D", "RES 1,E", "RES 1,H", "RES 1,L", "RES 1,(HL)", "RES 1,A", /*  88-8f   */
      "RES 2,B", "RES 2,C", "RES 2,D", "RES 2,E", "RES 2,H", "RES 2,L", "RES 2,(HL)", "RES 2,A", /*  90-97   */
      "RES 3,B", "RES 3,C", "RES 3,D", "RES 3,E", "RES 3,H", "RES 3,L", "RES 3,(HL)", "RES 3,A", /*  98-9f   */
      "RES 4,B", "RES 4,C", "RES 4,D", "RES 4,E", "RES 4,H", "RES 4,L", "RES 4,(HL)", "RES 4,A", /*  a0-a7   */
      "RES 5,B", "RES 5,C", "RES 5,D", "RES 5,E", "RES 5,H", "RES 5,L", "RES 5,(HL)", "RES 5,A", /*  a8-af   */
      "RES 6,B", "RES 6,C", "RES 6,D", "RES 6,E", "RES 6,H", "RES 6,L", "RES 6,(HL)", "RES 6,A", /*  b0-b7   */
      "RES 7,B", "RES 7,C", "RES 7,D", "RES 7,E", "RES 7,H", "RES 7,L", "RES 7,(HL)", "RES 7,A", /*  b8-bf   */
      "SET 0,B", "SET 0,C", "SET 0,D", "SET 0,E", "SET 0,H", "SET 0,L", "SET 0,(HL)", "SET 0,A", /*  c0-c7   */
      "SET 1,B", "SET 1,C", "SET 1,D", "SET 1,E", "SET 1,H", "SET 1,L", "SET 1,(HL)", "SET 1,A", /*  c8-cf   */
      "SET 2,B", "SET 2,C", "SET 2,D", "SET 2,E", "SET 2,H", "SET 2,L", "SET 2,(HL)", "SET 2,A", /*  d0-d7   */
      "SET 3,B", "SET 3,C", "SET 3,D", "SET 3,E", "SET 3,H", "SET 3,L", "SET 3,(HL)", "SET 3,A", /*  d8-df   */
      "SET 4,B", "SET 4,C", "SET 4,D", "SET 4,E", "SET 4,H", "SET 4,L", "SET 4,(HL)", "SET 4,A", /*  e0-e7   */
      "SET 5,B", "SET 5,C", "SET 5,D", "SET 5,E", "SET 5,H", "SET 5,L", "SET 5,(HL)", "SET 5,A", /*  e8-ef   */
      "SET 6,B", "SET 6,C", "SET 6,D", "SET 6,E", "SET 6,H", "SET 6,L", "SET 6,(HL)", "SET 6,A", /*  f0-f7   */
      "SET 7,B", "SET 7,C", "SET 7,D", "SET 7,E", "SET 7,H", "SET 7,L", "SET 7,(HL)", "SET 7,A") /*  f8-ff   */


  final val MnemonicsED: List[String] =
    List("DB EDh,00h", "DB EDh,01h", "DB EDh,02h", "DB EDh,03h", "DB EDh,04h", "DB EDh,05h", "DB EDh,06h", "DB EDh,07h", /*  00-07   */
      "DB EDh,08h", "DB EDh,09h", "DB EDh,0Ah", "DB EDh,0Bh", "DB EDh,0Ch", "DB EDh,0Dh", "DB EDh,0Eh", "DB EDh,0Fh", /*  08-0f   */
      "DB EDh,10h", "DB EDh,11h", "DB EDh,12h", "DB EDh,13h", "DB EDh,14h", "DB EDh,15h", "DB EDh,16h", "DB EDh,17h", /*  10-17   */
      "DB EDh,18h", "DB EDh,19h", "DB EDh,1Ah", "DB EDh,1Bh", "DB EDh,1Ch", "DB EDh,1Dh", "DB EDh,1Eh", "DB EDh,1Fh", /*  18-1f   */
      "DB EDh,20h", "DB EDh,21h", "DB EDh,22h", "DB EDh,23h", "DB EDh,24h", "DB EDh,25h", "DB EDh,26h", "DB EDh,27h", /*  20-27   */
      "DB EDh,28h", "DB EDh,29h", "DB EDh,2Ah", "DB EDh,2Bh", "DB EDh,2Ch", "DB EDh,2Dh", "DB EDh,2Eh", "DB EDh,2Fh", /*  28-2f   */
      "DB EDh,30h", "DB EDh,31h", "DB EDh,32h", "DB EDh,33h", "DB EDh,34h", "DB EDh,35h", "DB EDh,36h", "DB EDh,37h", /*  30-37   */
      "DB EDh,38h", "DB EDh,39h", "DB EDh,3Ah", "DB EDh,3Bh", "DB EDh,3Ch", "DB EDh,3Dh", "DB EDh,3Eh", "DB EDh,3Fh", /*  38-3f   */
      "IN B,(C)", "OUT (C),B", "SBC HL,BC", "LD (#h),BC", "NEG", "RETN", "IM 0", "LD I,A", /*  40-47   */
      "IN C,(C)", "OUT (C),C", "ADC HL,BC", "LD BC,(#h)", "DB EDh,4Ch", "RETI", "DB EDh,4Eh", "LD R,A", /*  48-4f   */
      "IN D,(C)", "OUT (C),D", "SBC HL,DE", "LD (#h),DE", "DB EDh,54h", "DB EDh,55h", "IM 1", "LD A,I", /*  50-57   */
      "IN E,(C)", "OUT (C),E", "ADC HL,DE", "LD DE,(#h)", "DB EDh,5Ch", "DB EDh,5Dh", "IM 2", "LD A,R", /*  58-5f   */
      "IN H,(C)", "OUT (C),H", "SBC HL,HL", "LD (#h),HL", "DB EDh,64h", "DB EDh,65h", "DB EDh,66h", "RRD", /*  60-67   */
      "IN L,(C)", "OUT (C),L", "ADC HL,HL", "LD HL,(#h)", "DB EDh,6Ch", "DB EDh,6Dh", "DB EDh,6Eh", "RLD", /*  68-6f   */
      "IN F,(C)", "DB EDh,71h", "SBC HL,SP", "LD (#h),SP", "DB EDh,74h", "DB EDh,75h", "DB EDh,76h", "DB EDh,77h", /*  70-77   */
      "IN A,(C)", "OUT (C),A", "ADC HL,SP", "LD SP,(#h)", "DB EDh,7Ch", "DB EDh,7Dh", "DB EDh,7Eh", "DB EDh,7Fh", /*  78-7f   */
      "DB EDh,80h", "DB EDh,81h", "DB EDh,82h", "DB EDh,83h", "DB EDh,84h", "DB EDh,85h", "DB EDh,86h", "DB EDh,87h", /*  80-87   */
      "DB EDh,88h", "DB EDh,89h", "DB EDh,8Ah", "DB EDh,8Bh", "DB EDh,8Ch", "DB EDh,8Dh", "DB EDh,8Eh", "DB EDh,8Fh", /*  88-8f   */
      "DB EDh,90h", "DB EDh,91h", "DB EDh,92h", "DB EDh,93h", "DB EDh,94h", "DB EDh,95h", "DB EDh,96h", "DB EDh,97h", /*  90-97   */
      "DB EDh,98h", "DB EDh,99h", "DB EDh,9Ah", "DB EDh,9Bh", "DB EDh,9Ch", "DB EDh,9Dh", "DB EDh,9Eh", "DB EDh,9Fh", /*  98-9f   */
      "LDI", "CPI", "INI", "OUTI", "DB EDh,A4h", "DB EDh,A5h", "DB EDh,A6h", "DB EDh,A7h", /*  a0-a7   */
      "LDD", "CPD", "IND", "OUTD", "DB EDh,ACh", "DB EDh,ADh", "DB EDh,AEh", "DB EDh,AFh", /*  a8-af   */
      "LDIR", "CPIR", "INIR", "OTIR", "DB EDh,B4h", "DB EDh,B5h", "DB EDh,B6h", "DB EDh,B7h", /*  b0-b7   */
      "LDDR", "CPDR", "INDR", "OTDR", "DB EDh,BCh", "DB EDh,BDh", "DB EDh,BEh", "DB EDh,BFh", /*  b8-bf   */
      "DB EDh,C0h", "DB EDh,C1h", "DB EDh,C2h", "DB EDh,C3h", "DB EDh,C4h", "DB EDh,C5h", "DB EDh,C6h", "DB EDh,C7h", /*  c0-c7   */
      "DB EDh,C8h", "DB EDh,C9h", "DB EDh,CAh", "DB EDh,CBh", "DB EDh,CCh", "DB EDh,CDh", "DB EDh,CEh", "DB EDh,CFh", /*  c8-cf   */
      "DB EDh,D0h", "DB EDh,D1h", "DB EDh,D2h", "DB EDh,D3h", "DB EDh,D4h", "DB EDh,D5h", "DB EDh,D6h", "DB EDh,D7h", /*  d0-d7   */
      "DB EDh,D8h", "DB EDh,D9h", "DB EDh,DAh", "DB EDh,DBh", "DB EDh,DCh", "DB EDh,DDh", "DB EDh,DEh", "DB EDh,DFh", /*  d8-df   */
      "DB EDh,E0h", "DB EDh,E1h", "DB EDh,E2h", "DB EDh,E3h", "DB EDh,E4h", "DB EDh,E5h", "DB EDh,E6h", "DB EDh,E7h", /*  e0-e7   */
      "DB EDh,E8h", "DB EDh,E9h", "DB EDh,EAh", "DB EDh,EBh", "DB EDh,ECh", "DB EDh,EDh", "DB EDh,EEh", "DB EDh,EFh", /*  e8-ef   */
      "DB EDh,F0h", "DB EDh,F1h", "DB EDh,F2h", "DB EDh,F3h", "DB EDh,F4h", "DB EDh,F5h", "DB EDh,F6h", "DB EDh,F7h", /*  f0-f7   */
      "DB EDh,F8h", "DB EDh,F9h", "DB EDh,FAh", "DB EDh,FBh", "DB EDh,FCh", "DB EDh,FDh", "DB EDh,FEh", "DB EDh,FFh") /*  f8-ff   */


  final val MnemonicsXX: List[String] =
    List("NOP", "LD BC,#h", "LD (BC),A", "INC BC", "INC B", "DEC B", "LD B,*h", "RLCA", /*  00-07   */
      "EX AF,AF'", "ADD I%,BC", "LD A,(BC)", "DEC BC", "INC C", "DEC C", "LD C,*h", "RRCA", /*  08-0f   */
      "DJNZ $h", "LD DE,#h", "LD (DE),A", "INC DE", "INC D", "DEC D", "LD D,*h", "RLA", /*  10-17   */
      "JR $h", "ADD I%,DE", "LD A,(DE)", "DEC DE", "INC E", "DEC E", "LD E,*h", "RRA", /*  18-1f   */
      "JR NZ,$h", "LD I%,#h", "LD (#h),I%", "INC I%", "INC I%H", "DEC I%H", "LD I%H,*h", "DAA", /*  20-27   */
      "JR Z,$h", "ADD I%,I%", "LD I%,(#h)", "DEC I%", "INC I%L", "DEC I%L", "LD I%L,*h", "CPL", /*  28-2f   */
      "JR NC,$h", "LD SP,#h", "LD (#h),A", "INC SP", "INC (I%+^h)", "DEC (I%+^h)", "LD (I%+^h),*h", "SCF", /*  30-37   */
      "JR C,$h", "ADD I%,SP", "LD A,(#h)", "DEC SP", "INC A", "DEC A", "LD A,*h", "CCF", /*  38-3f   */
      "LD B,B", "LD B,C", "LD B,D", "LD B,E", "LD B,I%H", "LD B,I%L", "LD B,(I%+^h)", "LD B,A", /*  40-47   */
      "LD C,B", "LD C,C", "LD C,D", "LD C,E", "LD C,I%H", "LD C,I%L", "LD C,(I%+^h)", "LD C,A", /*  48-4f   */
      "LD D,B", "LD D,C", "LD D,D", "LD D,E", "LD D,I%H", "LD D,I%L", "LD D,(I%+^h)", "LD D,A", /*  50-57   */
      "LD E,B", "LD E,C", "LD E,D", "LD E,E", "LD E,I%H", "LD E,I%L", "LD E,(I%+^h)", "LD E,A", /*  58-5f   */
      "LD I%H,B", "LD I%H,C", "LD I%H,D", "LD I%H,E", "LD I%H,I%H", "LD I%H,I%L", "LD H,(I%+^h)", "LD I%H,A", /*  60-67   */
      "LD I%L,B", "LD I%L,C", "LD I%L,D", "LD I%L,E", "LD I%L,I%H", "LD I%L,I%L", "LD L,(I%+^h)", "LD I%L,A", /*  68-6f   */
      "LD (I%+^h),B", "LD (I%+^h),C", "LD (I%+^h),D", "LD (I%+^h),E", "LD (I%+^h),H", "LD (I%+^h),L", "HALT", "LD (I%+^h),A", /*  70-77   */
      "LD A,B", "LD A,C", "LD A,D", "LD A,E", "LD A,I%H", "LD A,I%L", "LD A,(I%+^h)", "LD A,A", /*  78-7f   */
      "ADD A,B", "ADD A,C", "ADD A,D", "ADD A,E", "ADD A,I%H", "ADD A,I%L", "ADD A,(I%+^h)", "ADD A,A", /*  80-87   */
      "ADC A,B", "ADC A,C", "ADC A,D", "ADC A,E", "ADC A,I%H", "ADC A,I%L", "ADC A,(I%+^h)", "ADC A,A", /*  88-8f   */
      "SUB B", "SUB C", "SUB D", "SUB E", "SUB I%H", "SUB I%L", "SUB (I%+^h)", "SUB A", /*  90-97   */
      "SBC A,B", "SBC A,C", "SBC A,D", "SBC A,E", "SBC A,I%H", "SBC A,I%L", "SBC A,(I%+^h)", "SBC A,A", /*  98-9f   */
      "AND B", "AND C", "AND D", "AND E", "AND I%H", "AND I%L", "AND (I%+^h)", "AND A", /*  a0-a7   */
      "XOR B", "XOR C", "XOR D", "XOR E", "XOR I%H", "XOR I%L", "XOR (I%+^h)", "XOR A", /*  a8-af   */
      "OR B", "OR C", "OR D", "OR E", "OR I%H", "OR I%L", "OR (I%+^h)", "OR A", /*  b0-b7   */
      "CP B", "CP C", "CP D", "CP E", "CP I%H", "CP I%L", "CP (I%+^h)", "CP A", /*  b8-bf   */
      "RET NZ", "POP BC", "JP NZ,#h", "JP #h", "CALL NZ,#h", "PUSH BC", "ADD A,*h", "RST 00h", /*  c8-cf   */
      "RET Z", "RET", "JP Z,#h", "PFX_CB", "CALL Z,#h", "CALL #h", "ADC A,*h", "RST 08h", /*  c8-cf   */
      "RET NC", "POP DE", "JP NC,#h", "OUT (*h),A", "CALL NC,#h", "PUSH DE", "SUB *h", "RST 10h", /*  d0-d7   */
      "RET C", "EXX", "JP C,#h", "IN A,(*h)", "CALL C,#h", "PFX_DD", "SBC A,*h", "RST 18h", /*  d8-df   */
      "RET PO", "POP I%", "JP PO,#h", "EX (SP),I%", "CALL PO,#h", "PUSH I%", "AND *h", "RST 20h", /*  e0-e7   */
      "RET PE", "LD PC,I%", "JP PE,#h", "EX DE,I%", "CALL PE,#h", "PFX_ED", "XOR *h", "RST 28h", /*  e8-ef   */
      "RET P", "POP AF", "JP P,#h", "DI", "CALL P,#h", "PUSH AF", "OR *h", "RST 30h", /*  f0-f7   */
      "RET M", "LD SP,I%", "JP M,#h", "EI", "CALL M,#h", "PFX_FD", "CP *h", "RST 38h") /*  f8-ff   */


  final val MnemonicsXCB: List[String] =
    List("RLC B", "RLC C", "RLC D", "RLC E", "RLC H", "RLC L", "RLC (I%@h)", "RLC A", /*  00-07   */
      "RRC B", "RRC C", "RRC D", "RRC E", "RRC H", "RRC L", "RRC (I%@h)", "RRC A", /*  08-0f   */
      "RL B", "RL C", "RL D", "RL E", "RL H", "RL L", "RL (I%@h)", "RL A", /*  10-17   */
      "RR B", "RR C", "RR D", "RR E", "RR H", "RR L", "RR (I%@h)", "RR A", /*  18-1f   */
      "SLA B", "SLA C", "SLA D", "SLA E", "SLA H", "SLA L", "SLA (I%@h)", "SLA A", /*  20-27   */
      "SRA B", "SRA C", "SRA D", "SRA E", "SRA H", "SRA L", "SRA (I%@h)", "SRA A", /*  28-2f   */
      "SLL B", "SLL C", "SLL D", "SLL E", "SLL H", "SLL L", "SLL (I%@h)", "SLL A", /*  30-37   */
      "SRL B", "SRL C", "SRL D", "SRL E", "SRL H", "SRL L", "SRL (I%@h)", "SRL A", /*  38-3f   */
      "BIT 0,B", "BIT 0,C", "BIT 0,D", "BIT 0,E", "BIT 0,H", "BIT 0,L", "BIT 0,(I%@h)", "BIT 0,A", /*  40-47   */
      "BIT 1,B", "BIT 1,C", "BIT 1,D", "BIT 1,E", "BIT 1,H", "BIT 1,L", "BIT 1,(I%@h)", "BIT 1,A", /*  48-4f   */
      "BIT 2,B", "BIT 2,C", "BIT 2,D", "BIT 2,E", "BIT 2,H", "BIT 2,L", "BIT 2,(I%@h)", "BIT 2,A", /*  50-57   */
      "BIT 3,B", "BIT 3,C", "BIT 3,D", "BIT 3,E", "BIT 3,H", "BIT 3,L", "BIT 3,(I%@h)", "BIT 3,A", /*  58-5f   */
      "BIT 4,B", "BIT 4,C", "BIT 4,D", "BIT 4,E", "BIT 4,H", "BIT 4,L", "BIT 4,(I%@h)", "BIT 4,A", /*  60-67   */
      "BIT 5,B", "BIT 5,C", "BIT 5,D", "BIT 5,E", "BIT 5,H", "BIT 5,L", "BIT 5,(I%@h)", "BIT 5,A", /*  68-6f   */
      "BIT 6,B", "BIT 6,C", "BIT 6,D", "BIT 6,E", "BIT 6,H", "BIT 6,L", "BIT 6,(I%@h)", "BIT 6,A", /*  70-77   */
      "BIT 7,B", "BIT 7,C", "BIT 7,D", "BIT 7,E", "BIT 7,H", "BIT 7,L", "BIT 7,(I%@h)", "BIT 7,A", /*  78-7f   */
      "RES 0,B", "RES 0,C", "RES 0,D", "RES 0,E", "RES 0,H", "RES 0,L", "RES 0,(I%@h)", "RES 0,A", /*  80-87   */
      "RES 1,B", "RES 1,C", "RES 1,D", "RES 1,E", "RES 1,H", "RES 1,L", "RES 1,(I%@h)", "RES 1,A", /*  88-8f   */
      "RES 2,B", "RES 2,C", "RES 2,D", "RES 2,E", "RES 2,H", "RES 2,L", "RES 2,(I%@h)", "RES 2,A", /*  90-97   */
      "RES 3,B", "RES 3,C", "RES 3,D", "RES 3,E", "RES 3,H", "RES 3,L", "RES 3,(I%@h)", "RES 3,A", /*  98-9f   */
      "RES 4,B", "RES 4,C", "RES 4,D", "RES 4,E", "RES 4,H", "RES 4,L", "RES 4,(I%@h)", "RES 4,A", /*  a0-a7   */
      "RES 5,B", "RES 5,C", "RES 5,D", "RES 5,E", "RES 5,H", "RES 5,L", "RES 5,(I%@h)", "RES 5,A", /*  a8-af   */
      "RES 6,B", "RES 6,C", "RES 6,D", "RES 6,E", "RES 6,H", "RES 6,L", "RES 6,(I%@h)", "RES 6,A", /*  b0-b7   */
      "RES 7,B", "RES 7,C", "RES 7,D", "RES 7,E", "RES 7,H", "RES 7,L", "RES 7,(I%@h)", "RES 7,A", /*  b8-bf   */
      "SET 0,B", "SET 0,C", "SET 0,D", "SET 0,E", "SET 0,H", "SET 0,L", "SET 0,(I%@h)", "SET 0,A", /*  c0-c7   */
      "SET 1,B", "SET 1,C", "SET 1,D", "SET 1,E", "SET 1,H", "SET 1,L", "SET 1,(I%@h)", "SET 1,A", /*  c8-cf   */
      "SET 2,B", "SET 2,C", "SET 2,D", "SET 2,E", "SET 2,H", "SET 2,L", "SET 2,(I%@h)", "SET 2,A", /*  d0-d7   */
      "SET 3,B", "SET 3,C", "SET 3,D", "SET 3,E", "SET 3,H", "SET 3,L", "SET 3,(I%@h)", "SET 3,A", /*  d8-df   */
      "SET 4,B", "SET 4,C", "SET 4,D", "SET 4,E", "SET 4,H", "SET 4,L", "SET 4,(I%@h)", "SET 4,A", /*  e0-e7   */
      "SET 5,B", "SET 5,C", "SET 5,D", "SET 5,E", "SET 5,H", "SET 5,L", "SET 5,(I%@h)", "SET 5,A", /*  e8-ef   */
      "SET 6,B", "SET 6,C", "SET 6,D", "SET 6,E", "SET 6,H", "SET 6,L", "SET 6,(I%@h)", "SET 6,A", /*  f0-f7   */
      "SET 7,B", "SET 7,C", "SET 7,D", "SET 7,E", "SET 7,H", "SET 7,L", "SET 7,(I%@h)", "SET 7,A") /*  f8-ff   */


}