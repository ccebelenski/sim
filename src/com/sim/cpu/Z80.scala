package com.sim.cpu

import com.sim.{SimTimer, Utils}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  name = "Z80"
  override val MMU: BasicMMU = new Z80MMU(this)

  override def init(): Unit = ???

  var tStates: Long = 0L


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
  val IX = new Register16("IX")
  val IY = new Register16("IY")
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

      instr.byteValue match {

        case (0x00) => // NOP
          tStates = tStates + 4
        case (0x01) => // LD BC, nnnn
          tStates = tStates + 10
          BC(MMU.get16(PC))
          PC(UShort((PC + 2).shortValue()))
        case (0x02) => // LD (BC),A
          tStates = tStates + 7
          memoryBreak = CHECK_BREAK_BYTE(BC)
          if (!memoryBreak) MMU.put8(BC, A)
        case (0x03) => // INC BC
          tStates = tStates + 6
          BC(BC + 1)
        case (0x04) => // INC B
          tStates = tStates + 4
          B(B + 1)
          AF((AF & ~0xfe) | incTable(B) | SET_PV2(0x80, B))
        case (0x05) => // DEC B
          tStates = tStates + 4
          B.set8((B - UByte(1)).toUByte)
          AF((AF & ~0xfe) | decTable(B) | SET_PV2(0x7f, B))
        case (0x06) => // LD B,nn
          tStates = tStates + 7
          B(MMU.get8(PC))
          PC.increment()
        case (0x07) => // RLCA
          tStates = tStates + 4
          AF(((AF >> 7) & 0x0128) | ((AF << 1) & ~0x1ff) | (AF & 0xc4) | ((AF >> 15) & 1))
        case (0x08) => // EX AF, AF'
          tStates = tStates + 4
          AF.swap(AFP)
        case (0x09) => // ADD HL, BC
          tStates = tStates + 11
          val sum = HL + BC
          AF((AF & ~0x3b) | ((sum >> 8) & 0x28) | cbitsTable((HL ^ BC ^ sum) >> 8))
          HL(sum)
        case (0x0a) => // LD A, BC
          tStates = tStates + 7
          memoryBreak = CHECK_BREAK_BYTE(BC)
          if (!memoryBreak) A(MMU.get8(BC))
        case (0x0b) => // DEC BC
          tStates = tStates + 6
          BC.decrement()
        case (0x0c) => // INC C
          tStates = tStates + 4
          C.increment()
          AF((AF.get16 & ~0xfe) | incTable(C) | SET_PV2(0x80, C))

        case (0x76) => // HALT
          tStates = tStates + 4
          PC(PC - 1)
          execute = false
      }


      SimTimer.sim_interval = SimTimer.sim_interval - 1


    } // end SwitchCPUNow

    // TODO simulation halted
    Utils.outln(s"$name: Halted.")
    Utils.outln(showRegisters())
  }

  @inline
  private def INCR(count: Int): Unit = {
    R.set8(UByte(((R.get8 & ~0x7f) | ((R.get8 + count) & 0x7f)).toByte)) // Increment R
  }

  @inline
  // TODO Implement memory break points.
  private def CHECK_BREAK_BYTE(reg: Register16): Boolean = {
    false
  }

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

}
