package com.sim.cpu

import com.sim.{SimTimer, Utils}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

class Z80(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  name = "Z80"
  override val MMU: BasicMMU = new Z80MMU(this)

  override def init(): Unit = ???

  override def resetCPU(): Unit = {

    // Set up basic memory

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
  val HL = new LittleEndianCompositeRegister16("HL", H, L)
  val AF = new LittleEndianCompositeRegister16("HL", A, F)
  val BC = new LittleEndianCompositeRegister16("HL", B, C)
  val DE = new LittleEndianCompositeRegister16("HL", D, E)
  val HLP = new LittleEndianCompositeRegister16("HL", HP, LP)
  val AFP = new LittleEndianCompositeRegister16("HL", AP, FP)
  val BCP = new LittleEndianCompositeRegister16("HL", BP, CP)
  val DEP = new LittleEndianCompositeRegister16("HL", DP, EP)
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

  override def showRegisters(): Unit = ???

  override def runcpu(): Unit = {

    // tStates contains the number of t-states executed.  1 t-state is executed in 1 microsecond
    // on a 1Mhz CPU.  tStates is used for real-time simulations.
    val sliceLength = 10L
    var tStates: Long = 0L
    var tStatesInSlice: Long = sliceLength * clockFrequency // Number of t-states in 10mSec time-slice
    var startTime: Long = System.currentTimeMillis()
    var now: Long = 0L
    var tStateModifier: Boolean = false
    var switchCPUNow: Boolean = true
    var memoryBreak:Boolean = false

    while (switchCPUNow) {
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
        if (com.sim.Console.userInterrupt) switchCPUNow = false

      }

      // TODO Instruction execution

      INCR(1)
      val instr = MMU.get8(PC.get16)
      PC.set16(UShort((PC.get16 + 1).shortValue))

      instr.byteValue match {

        case(0x00) =>  // NOP
          tStates = tStates + 4
        case(0x01) => // LD BC, nnnn
          tStates = tStates + 10
          BC.set16(MMU.get16(PC))
          PC.set16(UShort((PC.get16 + 2).toShort))
        case(0x02) => // LD (BC),A
          tStates = tStates + 7
          memoryBreak = CHECK_BREAK_BYTE(BC)
          if(!memoryBreak) MMU.put8(BC,A)
        case (0x03) => // INC BC
          tStates = tStates + 6
          BC.set16(BC)
        case(0x04) => // INC B
          tStates = tStates + 4
          B.set8((B.get8 + UByte(1)).toUByte)


      }


      SimTimer.sim_interval = SimTimer.sim_interval - 1


    } // end SwitchCPUNow

    // TODO simulation halted
    Utils.outln(s"$name: Halted.  PC=${PC.get16.toHexString} SP=${SP.get16.toHexString} AF=${AF.get16.toHexString}")
  }

  @inline
  private def INCR(count: Int): Unit = {
    R.set8(UByte(((R.get8 & ~0x7f) | ((R.get8 + count) & 0x7f)).toByte)) // Increment R
  }

  @inline
  // TODO Implement memory break points.
  private def CHECK_BREAK_BYTE(reg:Register16) : Boolean = {
    false
  }

  val incTable = for(temp <- 0 to 255) yield {
    val t1 = (if((temp & 0xff) == 0) 1 else 0) << 6
    val t2 = (if((temp & 0xf) == 0) 1 else 0) << 4
    UByte(((temp & 0xa8) | (t1 | t2)).toByte)
  }
}
