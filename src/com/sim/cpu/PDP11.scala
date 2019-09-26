package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

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
  /* Protection modes */
  val MD_KER =         0
  val MD_SUP =         1
  val MD_UND  =        2
  val MD_USR   =       3

  val  PCQ_SIZE   =     64                              /* must be 2**n */
  val PCQ_MASK  =       (PCQ_SIZE - 1)
  def PCQ_ENTRY =      pcq[pcq_p = (pcq_p - 1) & PCQ_MASK] = PC
  def calc_is(md)  =    ((md) << VA_V_MODE)
  def calc_ds(md) =    (calc_is((md)) | ((MMR3 & dsmask[(md)])? VA_DS: 0))
  /* Register change tracking actually goes into variable reg_mods; from there
     it is copied into MMR1 if that register is not currently locked.  */

  def GET_SIGN_W(v:UShort) =  (((v) >> 15) & 1)
  def GET_SIGN_B(v:UByte) =   (((v) >> 7) & 1)
  def GET_Z(v)  =       ((v) == 0)
  def JMP_PC(x:Register16)  = {      PCQ_ENTRY
    PC(x)}
  def BRANCH_F(x) =  {   PCQ_ENTRY; PC ((PC + (((x) + (x)) & 0xff)) & 0xffff)}
  def BRANCH_B(x)  =  { PCQ_ENTRY; PC ((PC + (((x) + (x)) | 0xff00)) & 0xffff) }



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

  // Register file - Working registers
  val R:Array[Register16] = new Array[Register16](8)
  val REGFILE = Array.ofDim[Register16](6,2) /* R0-R5, two sets */
  val STACKFILE = new Array[Register16](4) // SP, 4 modes

  // TODO set up REGIFILE and STACKFILE

  // PSW Bits
  val BIT_C = 2 ^ 0 // Carry
  val BIT_V = 2 ^ 1 // Overflow
  val BIT_Z = 2 ^ 2 // Zero
  val BIT_N = 2 ^ 3 // Negative
  val BIT_TBIT = 2 ^ 4 // Trace Trap
  val BIT_IPL = 2 ^ 5 // IPL - 3 bits
  val BIT_FPD = 2 ^ 8 // First Part Done
  val BIT_NCF = 2 ^ 9 // Must Be Zero - 2 bits
  val BIT_RS = 2 ^ 11 // Register Set
  val BIT_PM = 2 ^ 12 // Previous access mode, 2 bits
  val BIT_CM = 2 ^ 14 // Current access mode, 2 bits

  /* Trap data structures */
  val VEC_RED = 0x4
  /* trap vectors */
  val VEC_ODD = 0x4
  val VEC_MME = 0xa8
  val VEC_NXM = 0x4
  val VEC_PAR = 0x4c
  val VEC_PRV = 0x4
  val VEC_ILL = 0x8
  val VEC_BPT = 0xc
  val VEC_IOT = 0x10
  val VEC_EMT = 0x18
  val VEC_TRAP = 0x1c
  val VEC_TRC = 0xc
  val VEC_YEL = 0x4
  val VEC_PWRFL = 0x14
  val VEC_FPE = 0xa4

  val trap_vec = ( /* trap req to vector */
    VEC_RED, VEC_ODD, VEC_MME, VEC_NXM,
    VEC_PAR, VEC_PRV, VEC_ILL, VEC_BPT,
    VEC_IOT, VEC_EMT, VEC_TRAP, VEC_TRC,
    VEC_YEL, VEC_PWRFL, VEC_FPE
  )

  /* Trap masks, descending priority order, following J-11
   An interrupt summary bit is kept with traps, to minimize overhead
*/

  val TRAP_V_RED = 0  /* red stk abort  4 */
  val TRAP_V_ODD = 1  /* odd address    4 */
  val TRAP_V_MME = 2  /* mem mgt      250 */
  val TRAP_V_NXM = 3  /* nx memory      4 */
  val TRAP_V_PAR = 4  /* parity err   114 */
  val TRAP_V_PRV = 5  /* priv inst      4 */
  val TRAP_V_ILL = 6  /* illegal inst  10 */
  val TRAP_V_BPT = 7  /* BPT           14 */
  val TRAP_V_IOT = 8  /* IOT           20 */
  val TRAP_V_EMT = 9  /* EMT           30 */
  val TRAP_V_TRAP = 10  /* TRAP          34 */
  val TRAP_V_TRC = 11  /* T bit         14 */
  val TRAP_V_YEL = 12  /* stack          4 */
  val TRAP_V_PWRFL = 13  /* power fail    24 */
  val TRAP_V_FPE = 14  /* fpe          244 */
  val TRAP_V_MAX = 15  /* intr = max trp # */
  val ABRT_V_BKPT = 16  /* stop due to breakpt */
  val TRAP_RED = (UInt(1) << TRAP_V_RED)
  val TRAP_ODD = (UInt(1) << TRAP_V_ODD)
  val TRAP_MME = (UInt(1) << TRAP_V_MME)
  val TRAP_NXM = (UInt(1) << TRAP_V_NXM)
  val TRAP_PAR = (UInt(1) << TRAP_V_PAR)
  val TRAP_PRV = (UInt(1) << TRAP_V_PRV)
  val TRAP_ILL = (UInt(1) << TRAP_V_ILL)
  val TRAP_BPT = (UInt(1) << TRAP_V_BPT)
  val TRAP_IOT = (UInt(1) << TRAP_V_IOT)
  val TRAP_EMT = (UInt(1) << TRAP_V_EMT)
  val TRAP_TRAP = (UInt(1) << TRAP_V_TRAP)
  val TRAP_TRC = (UInt(1) << TRAP_V_TRC)
  val TRAP_YEL = (UInt(1) << TRAP_V_YEL)
  val TRAP_PWRFL = (UInt(1) << TRAP_V_PWRFL)
  val TRAP_FPE = (UInt(1) << TRAP_V_FPE)
  val TRAP_INT = (UInt(1) << TRAP_V_MAX)
  val TRAP_ALL = ((UInt(1) << TRAP_V_MAX) - 1)
  /* all traps */
  val ABRT_BKPT = (UInt(1) << ABRT_V_BKPT)


  val trap_clear = ( /* trap clears */
    TRAP_RED + TRAP_PAR + TRAP_YEL + TRAP_TRC + TRAP_ODD + TRAP_NXM,
    TRAP_ODD + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_MME + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_NXM + TRAP_PAR + TRAP_YEL + TRAP_TRC,
    TRAP_PAR + TRAP_TRC, TRAP_PRV + TRAP_TRC,
    TRAP_ILL + TRAP_TRC, TRAP_BPT + TRAP_TRC,
    TRAP_IOT + TRAP_TRC, TRAP_EMT + TRAP_TRC,
    TRAP_TRAP + TRAP_TRC, TRAP_TRC,
    TRAP_YEL, TRAP_PWRFL, TRAP_FPE
  )


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

  override val registers: Map[String, Register] = ???
}
