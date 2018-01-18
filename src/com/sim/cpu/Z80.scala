package com.sim.cpu
import com.sim.unsigned.UInt

class Z80(isBanked:Boolean) extends BasicCPU(isBanked) {
  override val name: String = "Z80"
  override val MMU: BasicMMU = new Z80MMU(this)

  override def init(): Unit = ???

  override def resetCPU(): Unit = {

    // Set up basic memory

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
  val HL = new LittleEndianCompositeRegister16("HL", H,L)
  val AF = new LittleEndianCompositeRegister16("HL", A,F)
  val BC = new LittleEndianCompositeRegister16("HL", B,C)
  val DE = new LittleEndianCompositeRegister16("HL", D,E)
  val HLP = new LittleEndianCompositeRegister16("HL", HP,LP)
  val AFP = new LittleEndianCompositeRegister16("HL", AP,FP)
  val BCP = new LittleEndianCompositeRegister16("HL", BP,CP)
  val DEP = new LittleEndianCompositeRegister16("HL", DP,EP)
  val IX = new Register16("IX")
  val IY = new Register16("IY")
  val SP = new Register16("SP")
  val PC = new Register16("PC")

  override val registers: Map[String, Register] = Map("H" -> H, "L" -> L, "HL" -> HL)

  override def showRegisters(): Unit = ???
}
