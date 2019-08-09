package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.UInt

class PDP11(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "PDP11"
  override val MMU: PDP11MMU = new PDP11MMU(this)
  override val description: String = "PDP11 CPU"

  override def createUnitOptions: Unit = {
    // Set up CPU common options.
    super.createUnitOptions

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Break on HALT instruction.", value = false))
    unitOptions.append(ValueUnitOption("MEMORY", "Set the RAM size.", value = 0xFFFF))

  }
  override def showCommand(stringBuilder: StringBuilder): Unit = {
    super.showCommand(stringBuilder)
  }

  override def runcpu(singleStep: Boolean): Unit = ???

  override def runcpu(singleStep: Boolean, startAddr: UInt): Unit = {
    // Force the PC
    PC(startAddr.intValue)
    runcpu(singleStep)
  }

  // Registers
  val PC = new Register16("PC")
  val R0 = new Register16("R0")
  val R1 = new Register16("R1")
  val R2 = new Register16("R2")
  val R3 = new Register16("R3")
  val R4 = new Register16("R4")
  val R5 = new Register16("R5")
  val SP = new Register16("SP")
  val R00 = new Register16("R00")
  val R01 = new Register16("R01")
  val R02 = new Register16("R02")
  val R03 = new Register16("R03")
  val R04 = new Register16("R04")
  val R05 = new Register16("R05")
  val R10 = new Register16("R10")
  val R11 = new Register16("R11")
  val R12 = new Register16("R12")
  val R13 = new Register16("R13")
  val R14 = new Register16("R14")
  val R15 = new Register16("R15")
  val KSP = new Register16("KSP") // Kernel Stack Pointer
  val SSP = new Register16("SSP") // Supervisor Stack Pointer
  val USP = new Register16("USP") // User Stack Pointer
  val PSW = new Register16("PSW") // Processor status word - See bits above
  val PIRQ = new Register16("PIRQ") // Programmed Interrupt Request
  val STKLIM = new Register16("STKLIM") // Stack Limit
  // TODO FP






  // PSW Bits
  val BIT_C = 2^0 // Carry
  val BIT_V = 2^1 // Overflow
  val BIT_Z = 2^2 // Zero
  val BIT_N = 2^3 // Negative
  val BIT_TBIT = 2^4 // Trace Trap
  val BIT_IPL = 2^5 // IPL - 3 bits
  val BIT_FPD = 2^8 // First Part Done
  val BIT_NCF = 2^9 // Must Be Zero - 2 bits
  val BIT_RS = 2^11 // Register Set
  val BIT_PM = 2^12 // Previous access mode, 2 bits
  val BIT_CM = 2^14 // Current access mode, 2 bits



  override def onHalt(singleStepped: Boolean): Unit = ???

  //override val registers: Map[String, Register] = _

  override def resetCPU(): Unit = {
    PC(0)
    PSW(0)
    KSP(0)
    SSP(0)
    USP(0)
    R0(0)
    R00(0)
    R10(0)
    R1(0)
    R01(0)
    R11(0)
    R2(0)
    R02(0)
    R12(0)
    R3(0)
    R03(0)
    R13(0)
    R4(0)
    R04(0)
    R14(0)
    R5(0)
    R05(0)
    R15(0)


  }

  override def showRegisters(): String = ???

  override def showFlags(): String = ???

  override def DAsm(addr: Int, sb: StringBuilder): Int = ???

  override def init(): Unit = {} // TODO

  override def handles(value: UInt): Boolean = ???

  override def optionChanged(sb: StringBuilder): Unit = ???
}
