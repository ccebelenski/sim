package com.sim.cpu

import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

abstract class PDP11(isBanked: Boolean, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
  override val name = "PDP11"
  override val MMU: PDP11MMU = new PDP11MMU(this)
  override val description: String = "PDP11 CPU"

  override def createUnitOptions: Unit = {
    // Set up CPU common options.
    super.createUnitOptions

    unitOptions.append(BinaryUnitOption("BANKED", "Enable banked memory.", value = true))
    unitOptions.append(BinaryUnitOption("STOPONHALT", "Break on HALT instruction.", value = false))
    unitOptions.append(ValueUnitOption("MEMORY", "Set the RAM size.", value = (0xFFFF)))

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

  // cpu_type - what type of CPU - determines some features and functions
  // override this for specific cpu implementation
  val cpu_type : UInt // Model as bit mask
  val cpu_opt : UInt // cpu options
  val cpu_bme : UInt                                   /* bus map enable */
  val cpu_model : UInt                                /* CPU model */
  @inline def CPUT(x:UInt) : Boolean = (cpu_type & x) != 0
  @inline def CPUO(x:UInt) : Boolean = (cpu_opt & x) != 0
  @inline def UNIBUS : UInt = (cpu_opt & CPUOPT.BUS_U)

  /* Protection modes */
  val MD_KER =         0
  val MD_SUP =         1
  val MD_UND  =        2
  val MD_USR   =       3

  val STKLIM_RW =      UInt(0xff00)

  val  PCQ_SIZE   =     64                              /* must be 2**n */
  val PCQ_MASK  =       (PCQ_SIZE - 1)
  def PCQ_ENTRY =      {
    pcq_p = (pcqp - 1) & PCQ_MASK
    pcq(pcq_p) = PC
  }
  def calc_is(md:Int)  =    (md << VA_V_MODE)
  def calc_ds(md:Int) =    if(calc_is(md) | (MMU.MMR3 & dsmask(md))) VA_DS else 0
  /* Register change tracking actually goes into variable reg_mods; from there
     it is copied into MMR1 if that register is not currently locked.  */

  def GET_SIGN_W(v:UShort) =  (((v) >> 15) & 1)
  def GET_SIGN_B(v:UByte) =   (((v) >> 7) & 1)
  def GET_Z(v:Int)  =       ((v) == 0)
  def JMP_PC(x:Register16)  = {      PCQ_ENTRY
    PC.set16(x)
  }
  def BRANCH_F(x:UInt) = {
    PCQ_ENTRY
    PC.set16(PC.get16 + (((x) + (x)) & UInt(0xff)) & 0xffff)
  }
  def BRANCH_B(x:UInt)  = {
    PCQ_ENTRY
    PC.set16(PC.get16 + (((x) + (x)) | UInt(0xff00)) & 0xffff)
  }

  // Registers

  val R0 = new Register16("R0")
  val R1 = new Register16("R1")
  val R2 = new Register16("R2")
  val R3 = new Register16("R3")
  val R4 = new Register16("R4")
  val R5 = new Register16("R5")

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

  // Aliases for PC and SP
  @inline def PC: Register16 = R(6)
  @inline def SP: Register16 = R(7)

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
  val VEC_RED = UInt(0x4)
  /* trap vectors */
  val VEC_ODD = UInt(0x4)
  val VEC_MME = UInt(0xa8)
  val VEC_NXM = UInt(0x4)
  val VEC_PAR = UInt(0x4c)
  val VEC_PRV = UInt(0x4)
  val VEC_ILL = UInt(0x8)
  val VEC_BPT = UInt(0xc)
  val VEC_IOT = UInt(0x10)
  val VEC_EMT = UInt(0x18)
  val VEC_TRAP = UInt(0x1c)
  val VEC_TRC = UInt(0xc)
  val VEC_YEL = UInt(0x4)
  val VEC_PWRFL = UInt(0x14)
  val VEC_FPE = UInt(0xa4)

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
  val TRAP_RED: UInt = (UInt(1) << TRAP_V_RED)
  val TRAP_ODD: UInt = (UInt(1) << TRAP_V_ODD)
  val TRAP_MME: UInt = (UInt(1) << TRAP_V_MME)
  val TRAP_NXM: UInt = (UInt(1) << TRAP_V_NXM)
  val TRAP_PAR: UInt = (UInt(1) << TRAP_V_PAR)
  val TRAP_PRV: UInt = (UInt(1) << TRAP_V_PRV)
  val TRAP_ILL: UInt = (UInt(1) << TRAP_V_ILL)
  val TRAP_BPT: UInt = (UInt(1) << TRAP_V_BPT)
  val TRAP_IOT: UInt = (UInt(1) << TRAP_V_IOT)
  val TRAP_EMT: UInt = (UInt(1) << TRAP_V_EMT)
  val TRAP_TRAP: UInt = (UInt(1) << TRAP_V_TRAP)
  val TRAP_TRC: UInt = (UInt(1) << TRAP_V_TRC)
  val TRAP_YEL: UInt = (UInt(1) << TRAP_V_YEL)
  val TRAP_PWRFL: UInt = (UInt(1) << TRAP_V_PWRFL)
  val TRAP_FPE: UInt = (UInt(1) << TRAP_V_FPE)
  val TRAP_INT: UInt = (UInt(1) << TRAP_V_MAX)
  val TRAP_ALL: UInt = UInt((UInt(1) << TRAP_V_MAX) - 1)
  /* all traps */
  val ABRT_BKPT: UInt = (UInt(1) << ABRT_V_BKPT)


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

  // PSW
  var PSW :Int = 0
  var cm: Int = 0  /*   current mode */
  var pm: Int = 0   /*   previous mode */
  var rs: Int = 0   /*   register set */
  var fpd: Int = 0  /*   first part done */
  var ipl: Int = 0  /*   int pri level */
  var tbit: Int = 0 /*   trace flag */
  var N: Int = 0
    var Z: Int = 0
    var V: Int = 0
    var C: Int = 0 /*   condition codes */


  
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

  def set_stack_trap(adr: UInt): Unit = {
    if (CPUT(CPUOPT.HAS_STKLF)) {
      /* fixed stack? */
      setTRAP(TRAP_YEL) /* always yellow trap */
      setCPUERR(CPUOPT.CPUE_YEL)
    }
    else if (CPUT(CPUOPT.HAS_STKLR)) /* register limit? */
      if (adr.intValue >= (STKLIM + STKL_R)) {
      /* yellow zone? */
        setTRAP(TRAP_YEL) /* still yellow trap */
      setCPUERR(CPUE_YEL)
    }
    else {
      /* red zone abort */
        setCPUERR(CPUE_RED)
      STACKFILE(MD_KER).set16(4)
      SP.set16(4)
      ABORT(TRAP_RED)
    }
    /* no stack limit */
  }

  var trap_req:UInt = UInt(0) // Trap Requests
  @inline def setTRAP(name: UInt) = {trap_req = trap_req | name}
}

object PDP11 {
  /* Architectural constants */

  val STKL_R    =      UInt(0xe0)                            /* stack limit */
  val STKL_Y    =      UInt(0x100)
  val VASIZE    =      UInt(0x10000  )                       /* 2**16 */
  val VAMASK    =      (VASIZE - 1)                    /* 2**16 - 1 */
  val MEMSIZE64K =     UInt(0x10000)                         /* 2**16 */
  val UNIMEMSIZE  =    UInt(0x40000)                       /* 2**18 */
  val UNIMASK      =   (UNIMEMSIZE - 1)                /* 2**18 - 1 */
  val IOPAGEBASE    =  UInt(0x3fe000)                       /* 2**22 - 2**13 */
  val IOPAGESIZE     = UInt(0x2000  )                     /* 2**13 */
  val IOPAGEMASK      =(IOPAGESIZE - 1)                /* 2**13 - 1 */
  val MAXMEMSIZE  =    UInt(0x400000)                       /* 2**22 */
  val PAMASK      =    (MAXMEMSIZE - 1)                /* 2**22 - 1 */
  val DMASK       =    UInt(0xffff)
  val BMASK       =    UInt(0xff)

}
protected case class CPUTAB(var name : String, // Model name
                          var std: UInt, // standard flags
                          var opt: UInt, // set/clear flags
                          var              maxm: UInt, // max memory
                          var              psw: UInt, // PSW mask
                          var              mfpt: UInt, // MFPT code
                          var              par: UInt, // PAR mask
                          var              pdr: UInt, // PDR mask
                          var              mm0: UInt, // MMR0 mask
                          var              mm3 : UInt // MMR3 mask
                         ) {
}

protected object CPUOPT {

  val BUS_U : UInt   =       (UInt(1) << 0)                       /* Unibus */
  val BUS_Q :UInt    =      UInt(0)                             /* Qbus */
  /* CPU models */

  val MOD_1103  =      0
  val MOD_1104  =      1
  val MOD_1105  =      2
  val MOD_1120  =      3
  val MOD_1123  =      4
  val MOD_1123P =      5
  val MOD_1124  =      6
  val MOD_1134  =      7
  val MOD_1140  =      8
  val MOD_1144  =      9
  val MOD_1145  =      10
  val MOD_1160  =      11
  val MOD_1170  =      12
  val MOD_1173  =      13
  val MOD_1153  =      14
  val MOD_1173B =      15
  val MOD_1183  =      16
  val MOD_1184  =      17
  val MOD_1193  =      18
  val MOD_1194  =      19
  val MOD_T      =     20

  val CPUT_03: UInt =    (UInt(1) << MOD_1103)                /* LSI-11 */
  val CPUT_04: UInt =    (UInt(1) << MOD_1104)                /* 11/04 */
  val CPUT_05: UInt =    (UInt(1) << MOD_1105)                /* 11/05 */
  val CPUT_20: UInt =    (UInt(1) << MOD_1120)                /* 11/20 */
  val CPUT_23: UInt =    (UInt(1) << MOD_1123)                /* 11/23 */
  val CPUT_23P: UInt =    (UInt(1) << MOD_1123P)               /* 11/23+ */
  val CPUT_24: UInt =    (UInt(1) << MOD_1124)                /* 11/24 */
  val CPUT_34: UInt =    (UInt(1) << MOD_1134)                /* 11/34 */
  val CPUT_40: UInt =    (UInt(1) << MOD_1140)                /* 11/40 */
  val CPUT_44: UInt =    (UInt(1) << MOD_1144)                /* 11/44 */
  val CPUT_45: UInt =    (UInt(1) << MOD_1145)                /* 11/45 */
  val CPUT_60: UInt =    (UInt(1) << MOD_1160)                /* 11/60 */
  val CPUT_70: UInt =    (UInt(1) << MOD_1170)                /* 11/70 */
  val CPUT_73: UInt =    (UInt(1) << MOD_1173)                /* 11/73 */
  val CPUT_53: UInt =    (UInt(1) << MOD_1153)                /* 11/53 */
  val CPUT_73B: UInt =    (UInt(1) << MOD_1173B)               /* 11/73B */
  val CPUT_83: UInt =    (UInt(1) << MOD_1183)                /* 11/83 */
  val CPUT_84: UInt =    (UInt(1) << MOD_1184)                /* 11/84 */
  val CPUT_93: UInt =    (UInt(1) << MOD_1193)                /* 11/93 */
  val CPUT_94: UInt =    (UInt(1) << MOD_1194)                /* 11/94 */
  val CPUT_T: UInt =    (UInt(1) << MOD_T)                   /* T-11 */

  val CPUT_F: UInt =    (CPUT_23|CPUT_23P|CPUT_24)      /* all F11's */
  val CPUT_J: UInt =    (CPUT_53|CPUT_73|CPUT_73B|CPUT_83|CPUT_84|CPUT_93|CPUT_94)
  val CPUT_JB: UInt =    (CPUT_73B|CPUT_83|CPUT_84)      /* KDJ11B */
  val CPUT_JE: UInt =    (CPUT_93|CPUT_94)               /* KDJ11E */
  val CPUT_JU: UInt =    (CPUT_84|CPUT_94)               /* KTJ11B UBA */
  val CPUT_ALL    =    UInt(0xFFFFFFFF)


  /* CPU options */

  //val BUS_U : UInt   =       (UInt(1) << 0)                       /* Unibus */
  //val BUS_Q :UInt    =      UInt(0)                             /* Qbus */
  val OPT_EIS :UInt   =     (UInt(1) << 1)                       /* EIS */
  val OPT_FIS :UInt    =    (UInt(1) << 2)                       /* FIS */
  val OPT_FPP :UInt     =   (UInt(1) << 3)                       /* FPP */
  val OPT_CIS :UInt    =    (UInt(1) << 4)                       /* CIS */
  val OPT_MMU: UInt =    (UInt(1) << 5)                       /* MMU */
  val OPT_RH11 :UInt   =    (UInt(1) << 6)                       /* RH11 */
  val OPT_PAR :UInt    =    (UInt(1) << 7)                       /* parity */
  val OPT_UBM :UInt    =    (UInt(1) << 8)                       /* UBM */
  val OPT_BVT :UInt    =    (UInt(1) << 9)                       /* BEVENT */
  /* Feature sets
  SDSD                 source addr, dest addr, source fetch, dest fetch
    SR                   switch register
  DR                   display register
  RTT                  RTT instruction
  SXS                  SXT, XOR, SOB instructions
    MARK                 MARK instruction
  SPL                  SPL instruction
  MXPY                 MTPI, MTPD, MFPI, MFPD instructions
    MXPS                 MTPS, MFPS instructions
    MFPT                 MFPT instruction
  CSM                  CSM instruction
  TSWLK                TSTSET, WRLCK instructions
    PSW                  PSW register
  EXPT                 explicit PSW writes can alter T-bit
  IOSR                 general registers readable from programs in IO space
  2REG                 dual register set
  MMR3                 MMR3 register
  MMTR                 mem mgt traps
    STKLR                STKLIM register
  STKLF                fixed stack limit
    SID                  supervisor mode, I/D spaces
    ODD                  odd address trap
    HALT4                halt in kernel mode traps to 4
  JREG4                JMP/JSR R traps to 4
  STKA                 stop on stack abort
  LTCR                 LTC CSR
  LTCM                 LTC CSR<7>
    */

  val IS_SDSD: UInt =    (CPUT_20|CPUT_F|CPUT_40|CPUT_60|CPUT_J|CPUT_T)
  val HAS_SR: UInt =   (CPUT_04|CPUT_05|CPUT_20|CPUT_34|CPUT_40|     CPUT_44|CPUT_45|CPUT_60|CPUT_70)
  val HAS_DR: UInt =      (CPUT_04|CPUT_05|CPUT_20|CPUT_24|CPUT_34|     CPUT_40|CPUT_44|CPUT_45|CPUT_60|CPUT_70)
  val HAS_RTT: UInt =       (CPUT_03|CPUT_04|CPUT_F|CPUT_34|CPUT_40|     CPUT_44|CPUT_45|CPUT_60|CPUT_70|CPUT_J|CPUT_T)
  val HAS_SXS: UInt =     (CPUT_03|CPUT_F|CPUT_34|CPUT_40|CPUT_44|     CPUT_45|CPUT_60|CPUT_70|CPUT_J|CPUT_T)
  val HAS_MARK: UInt =      (CPUT_03|CPUT_F|CPUT_34|CPUT_40|CPUT_44|     CPUT_45|CPUT_60|CPUT_70|CPUT_J)
  val HAS_SPL: UInt =       (CPUT_44|CPUT_45|CPUT_70|CPUT_J)
  val HAS_MXPY: UInt =    (CPUT_F|CPUT_34|CPUT_40|CPUT_44|CPUT_45|     CPUT_60|CPUT_70|CPUT_J)
  val HAS_MXPS: UInt =    (CPUT_03|CPUT_F|CPUT_34|CPUT_J|CPUT_T)
  val HAS_MFPT: UInt =     (CPUT_F|CPUT_44|CPUT_J|CPUT_T)
  val HAS_CSM: UInt =     (CPUT_44|CPUT_J)
  val HAS_TSWLK: UInt =     (CPUT_J)
  val HAS_PSW: UInt =     (CPUT_04|CPUT_05|CPUT_20|CPUT_F|CPUT_34|CPUT_40|     CPUT_44|CPUT_45|CPUT_60|CPUT_70|CPUT_J)
  val HAS_EXPT: UInt =     (CPUT_04|CPUT_05|CPUT_20)
  val HAS_IOSR: UInt =     (CPUT_04|CPUT_05)
  val HAS_2REG: UInt =     (CPUT_45|CPUT_70|CPUT_J)
  val HAS_MMR3: UInt =     (CPUT_F|CPUT_44|CPUT_45|CPUT_70|CPUT_J)
  val HAS_MMTR: UInt =      (CPUT_45|CPUT_70)
  val HAS_STKLR: UInt =   (CPUT_45|CPUT_60|CPUT_70)
  val HAS_STKLF: UInt =    (CPUT_04|CPUT_05|CPUT_20|CPUT_F|CPUT_34|     CPUT_40|CPUT_44|CPUT_J)
  val HAS_SID: UInt =       (CPUT_44|CPUT_45|CPUT_70|CPUT_J)
  val HAS_ODD: UInt =      (CPUT_04|CPUT_05|CPUT_20|CPUT_34|CPUT_40|     CPUT_44|CPUT_45|CPUT_60|CPUT_70|CPUT_J)
  val HAS_HALT4: UInt =    (CPUT_44|CPUT_45|CPUT_70|CPUT_J)
  val HAS_JREG4: UInt =     (CPUT_03|CPUT_04|CPUT_05|CPUT_20|CPUT_F|     CPUT_34|CPUT_40|CPUT_60|CPUT_T)
  val STOP_STKA: UInt =     (CPUT_03|CPUT_04|CPUT_05|CPUT_20|CPUT_34|CPUT_44)
  val HAS_LTCR: UInt =     (CPUT_04|CPUT_05|CPUT_20|CPUT_23P|CPUT_24|     CPUT_34|CPUT_40|CPUT_44|CPUT_45|CPUT_60|     CPUT_70|CPUT_J)
  val HAS_LTCM: UInt =     (CPUT_04|CPUT_05|CPUT_20|CPUT_24|CPUT_34|     CPUT_40|CPUT_44|CPUT_45|CPUT_60|CPUT_70|CPUT_J)

  val SOP_1103   =     (BUS_Q|OPT_BVT)
  val OPT_1103   =     (OPT_EIS|OPT_FIS|OPT_BVT)
  val PSW_1103   =     UInt(0xff)

  val SOP_1104    =    (BUS_U)
  val OPT_1104    =    UInt(0)
  val PSW_1104    =    UInt(0xff)

  val SOP_1105    =    (BUS_U)
  val OPT_1105    =    UInt(0)
  val PSW_1105    =    UInt(0xff)

  val SOP_1120    =    (BUS_U)
  val OPT_1120    =    UInt(0)
  val PSW_1120    =    UInt(0xff)

  val SOP_1123    =    (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU|OPT_BVT)
  val OPT_1123    =    (OPT_FPP|OPT_CIS|OPT_BVT)
  val PSW_F       =    UInt(0xf1ff)
  val PAR_F       =    UInt(0xffff)
  val PDR_F       =    UInt(0x7f4e)
  val MM0_F       =    UInt(0xe06f)
  val MM3_F       =    UInt(0x30)

  val SOP_1123P    =   (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1123P   =    (OPT_FPP|OPT_CIS)

  val SOP_1124    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_UBM)
  val OPT_1124   =     (OPT_FPP|OPT_CIS)

  val SOP_1134    =    (BUS_U|OPT_EIS|OPT_MMU)
  val OPT_1134    =    (OPT_FPP)
  val PSW_1134    =    UInt(0xf0ff)
  val PAR_1134    =    UInt(0xfff)
  val PDR_1134    =    UInt(0x7f4e)
  val MM0_1134   =     UInt(0xe16f)

  val SOP_1140   =     (BUS_U|OPT_EIS|OPT_MMU)
  val OPT_1140     =   (OPT_FIS)
  val PSW_1140   =     UInt(0xf0ff)
  val PAR_1140   =     UInt(0xfff)
  val PDR_1140    =    UInt(0x7f4e)
  val MM0_1140   =     UInt(0xe16f)

  val SOP_1144    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_UBM)
  val OPT_1144    =    (OPT_FPP|OPT_CIS)
  val PSW_1144    =    UInt(0xf1ff)
  val PAR_1144   =     UInt(0xffff)
  val PDR_1144   =     UInt(0xff4e)
  val MM0_1144   =     UInt(0xe16f)
  val MM3_1144   =     UInt(0x3f)

  val SOP_1145    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_RH11)
  val OPT_1145   =     (OPT_FPP)
  val PSW_1145   =     UInt(0xf8ff)
  val PAR_1145   =     UInt(0xfff)
  val PDR_1145   =     UInt(0x7fcf)
  val MM0_1145    =    UInt(0xf3ff)
  val MM3_1145    =    UInt(0x7)

  val SOP_1160    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1160    =    UInt(0)
  val PSW_1160    =    UInt(0xf0ff)
  val PAR_1160    =    UInt(0xfff)
  val PDR_1160    =    UInt(0x7f4e)
  val MM0_1160    =    UInt(0xe16f)

  val SOP_1170    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_UBM)
  val OPT_1170    =    (OPT_FPP|OPT_RH11)
  val PSW_1170    =    UInt(0xf8ff)
  val PAR_1170    =    UInt(0xffff)
  val PDR_1170    =    UInt(0x7fcf)
  val MM0_1170    =    UInt(0xf3ff)
  val MM3_1170     =   UInt(0x37)

  val SOP_1173   =     (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1173  =      (OPT_CIS)
  val PSW_J     =      UInt(0xf9ff)
  val PAR_J     =      UInt(0xffff)
  val PDR_J      =     UInt(0xff4e)
  val MM0_J      =     UInt(0xe07f)
  val MM3_J      =     UInt(0x3f)

  val SOP_1153    =    (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1153   =     (OPT_CIS)

  val SOP_1173B   =    (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1173B   =    (OPT_CIS)

  val SOP_1183     =   (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1183    =    (OPT_CIS)

  val SOP_1184    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_UBM|OPT_RH11)
  val OPT_1184     =   (OPT_CIS)

  val SOP_1193     =   (BUS_Q|OPT_EIS|OPT_FPP|OPT_MMU)
  val OPT_1193    =   (OPT_CIS)

  val SOP_1194    =    (BUS_U|OPT_EIS|OPT_FPP|OPT_MMU|OPT_UBM|OPT_RH11)
  val OPT_1194   =     (OPT_CIS)

  /* MFPT codes */

  val MFPT_44     =    UInt(1)
  val MFPT_F      =    UInt(3)
  val MFPT_T      =    UInt(4)
  val MFPT_J      =    UInt(5)

  val cpu_tab : Array[CPUTAB] = Array(
    CPUTAB( "11/03", SOP_1103, OPT_1103, PDP11.MEMSIZE64K, PSW_1103,UInt(0),UInt(0),UInt(0),UInt(0), UInt(0)),
    CPUTAB( "11/04", SOP_1104, OPT_1104, PDP11.MEMSIZE64K, PSW_1104,UInt(0),UInt(0),UInt(0),UInt(0), UInt(0)),
    CPUTAB( "11/05", SOP_1105, OPT_1105, PDP11.MEMSIZE64K, PSW_1105,UInt(0),UInt(0),UInt(0),UInt(0), UInt(0)),
    CPUTAB( "11/20", SOP_1120, OPT_1120, PDP11.MEMSIZE64K, PSW_1120,UInt(0),UInt(0),UInt(0),UInt(0), UInt(0)),
    CPUTAB( "11/23", SOP_1123, OPT_1123, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F ),
    CPUTAB( "11/23+", SOP_1123P, OPT_1123P, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F ),
    CPUTAB( "11/24", SOP_1124, OPT_1124, PDP11.MAXMEMSIZE, PSW_F, MFPT_F, PAR_F, PDR_F, MM0_F, MM3_F ),
    CPUTAB( "11/34", SOP_1134, OPT_1134, PDP11.UNIMEMSIZE, PSW_1134,UInt(0), PAR_1134, PDR_1134, MM0_1134, UInt(0)),
    CPUTAB( "11/40", SOP_1140, OPT_1140, PDP11.UNIMEMSIZE, PSW_1140,UInt(0), PAR_1140, PDR_1140, MM0_1140, UInt(0)),
    CPUTAB( "11/44", SOP_1144, OPT_1144, PDP11.MAXMEMSIZE, PSW_1144,MFPT_44, PAR_1144, PDR_1144, MM0_1144, MM3_1144 ),
    CPUTAB( "11/45", SOP_1145, OPT_1145, PDP11.UNIMEMSIZE, PSW_1145,UInt(0), PAR_1145, PDR_1145, MM0_1145, MM3_1145 ),
    CPUTAB( "11/60", SOP_1160, OPT_1160, PDP11.UNIMEMSIZE, PSW_1160,UInt(0), PAR_1160, PDR_1160, MM0_1160, UInt(0)),
    CPUTAB( "11/70", SOP_1170, OPT_1170, PDP11.MAXMEMSIZE, PSW_1170,UInt(0), PAR_1170, PDR_1170, MM0_1170, MM3_1170),
    CPUTAB( "11/73", SOP_1173, OPT_1173, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/53", SOP_1153, OPT_1153, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/73B", SOP_1173B, OPT_1173B, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/83", SOP_1183, OPT_1183, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/84", SOP_1184, OPT_1184, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/93", SOP_1193, OPT_1193, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J ),
    CPUTAB( "11/94", SOP_1194, OPT_1194, PDP11.MAXMEMSIZE, PSW_J,MFPT_J, PAR_J, PDR_J, MM0_J, MM3_J )
  )

}