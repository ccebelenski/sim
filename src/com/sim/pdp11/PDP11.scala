package com.sim.pdp11

import com.sim.cpu._
import com.sim.device.{BinaryUnitOption, ValueUnitOption}
import com.sim.machine.AbstractMachine
import com.sim.unsigned.{UByte, UInt, UShort}

abstract class PDP11(isBanked: Boolean = false, override val machine: AbstractMachine) extends BasicCPU(isBanked, machine) {
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
  val cpu_type: UInt // Model as bit mask
  val cpu_opt: UInt // cpu options
  var cpu_bme: Boolean
  /* bus map enable */
  val cpu_model: UInt

  /* CPU model */
  @inline def CPUT(x: UInt): Boolean = (cpu_type & x) != 0

  @inline def CPUO(x: UInt): Boolean = (cpu_opt & x) != 0

  @inline def UNIBUS: UInt = (cpu_opt & CPUOPT.BUS_U)

  /*
  Traps and interrupts.  Variable trap_req bit-encodes all possible
  traps.  In addition, an interrupt pending bit is encoded as the
  lowest priority trap.  Traps are processed by trap_vec and trap_clear,
  which provide the vector and subordinate traps to clear, respectively.
    Array int_req[0:7] bit encodes all possible interrupts.  It is masked
  under the interrupt priority level, ipl.  If any interrupt request
    is not masked, the interrupt bit is set in trap_req.  While most
    interrupts are handled centrally, a device can supply an interrupt
    acknowledge routine.
    */

  val int_req = new Array[Int](PDP11.IPL_HLVL)

  val PCQ_SIZE: Int = 64
  /* must be 2**n */
  val pcq: Array[Register16] = new Array[Register16](PCQ_SIZE);
  /* PC queue */
  var pcq_p: Int = 0
  val PCQ_MASK: Int = {PCQ_SIZE - 1}

  def PCQ_ENTRY(): Unit = {
    pcq_p = (pcq_p - 1) & PCQ_MASK
    pcq(pcq_p) = PC
  }

  /* Protection modes */
  val MD_KER = 0
  val MD_SUP = 1
  val MD_UND = 2
  val MD_USR = 3

  val STKLIM_RW = UInt(0xff00)

  /* Register change tracking actually goes into variable reg_mods; from there
     it is copied into MMR1 if that register is not currently locked.  */

  def GET_SIGN_W(v: UShort): Int = {(v >> 15) & 1}

  def GET_SIGN_B(v: UByte): Int = {(v >> 7) & 1}

  def GET_Z(v: Int): Boolean = {v == 0}

  def JMP_PC(x: Register16): Unit = {
    PCQ_ENTRY()
    PC.set16(x)
  }

  def BRANCH_F(x: UInt): Unit = {
    PCQ_ENTRY()
    PC.set16(PC.get16 + ((x + x) & UInt(0xff)) & 0xffff)
  }

  def BRANCH_B(x: UInt): Unit = {
    PCQ_ENTRY()
    PC.set16(PC.get16 + ((x + x) | UInt(0xff00)) & 0xffff)
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
  //  val PSW = new Register16("PSW") // Processor status word - See bits above
  val PIRQ = new Register16("PIRQ") // Programmed Interrupt Request
  val STKLIM = new Register16("STKLIM") // Stack Limit
  // TODO FP

  // Register file - Working registers
  val R: Array[Register16] = new Array[Register16](8)

  // Aliases for PC and SP
  @inline def PC: Register16 = R(6)

  @inline def SP: Register16 = R(7)

  val REGFILE: Array[Array[Register16]] = Array.ofDim[Register16](6, 2)
  /* R0-R5, two sets */
  val STACKFILE = new Array[Register16](4) // SP, 4 modes

  // TODO set up REGIFILE and STACKFILE


  // PSW
  var PSW: Int = 0
  var cm: Int = 0
  /*   current mode */
  var pm: Int = 0
  /*   previous mode */
  var rs: Int = 0
  /*   register set */
  var fpd: Int = 0
  /*   first part done */
  var ipl: Int = 0
  /*   int pri level */
  var tbit: Int = 0
  /*   trace flag */
  var N: Int = 0
  var Z: Int = 0
  var V: Int = 0
  var C: Int = 0 /*   condition codes */


  override def onHalt(singleStepped: Boolean): Unit = ???

  //override val registers: Map[String, Register] = _
  override def resetCPU(): Unit = {

    PIRQ.set16(0)
    STKLIM.set16(0)
    if (CPUT (CPUOPT.CPUT_T))                                      /* T11? */
      PSW = 0xe0                                       /* start at IPL 7 */
    else
      PSW = 0                                            /* else at IPL 0 */
    MMU.MMR0.set16(0)
    MMU.MMR1.set16(0)
    MMU.MMR2.set16(0)
    MMU.MMR3.set16(0)

    trap_req = 0;
    wait_state = 0;
    if (M == NULL) {                    /* First time init */
      M = (uint16 *) calloc (MEMSIZE >> 1, sizeof (uint16));
      if (M == NULL)
        return SCPE_MEM;
      //sim_set_pchar (0, "01000023640"); /* ESC, CR, LF, TAB, BS, BEL, ENQ */
      //sim_brk_dflt = SWMASK ('E');
      //sim_brk_types = sim_brk_dflt|SWMASK ('P')|
      //  SWMASK ('R')|SWMASK ('S')|
      //  SWMASK ('W')|SWMASK ('X');
      //sim_brk_type_desc = cpu_breakpoints;
      //sim_vm_is_subroutine_call = &cpu_is_pc_a_subroutine_call;
      sim_clock_precalibrate_commands = pdp11_clock_precalibrate_commands;
      auto_config(NULL, 0);           /* do an initial auto configure */
    }
    pcq_r = find_reg ("PCQ", NULL, dptr);
    if (pcq_r)
      pcq_r->qptr = 0;
    else
      return SCPE_IERR;
    //set_r_display (0, MD_KER);
    return build_dib_tab ();            /* build, chk dib_tab */
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
      setTRAP(PDP11.TRAP_YEL.intValue) /* always yellow trap */
      setCPUERR(PDP11.CPUE_YEL)
    }
    else if (CPUT(CPUOPT.HAS_STKLR)) /* register limit? */
      if (adr.intValue >= (STKLIM.get16 + PDP11.STKL_R).intValue) {
        /* yellow zone? */
        setTRAP(PDP11.TRAP_YEL.intValue) /* still yellow trap */
        setCPUERR(PDP11.CPUE_YEL)
      }
      else {
        /* red zone abort */
        setCPUERR(PDP11.CPUE_RED)
        STACKFILE(MD_KER).set16(4)
        SP.set16(4)
        throw AbortException(PDP11.TRAP_RED)
      }
    /* no stack limit */
  }

  var trap_req: Int = 0 // Trap Requests
  @inline def setTRAP(name: Int): Unit = {
    trap_req = trap_req | name
  }

  @inline def setCPUERR(name: UInt): Unit = CPUERR = CPUERR | name
}

object PDP11 {
  /* Architectural constants */

  val STKL_R = UInt(0xe0)
  /* stack limit */
  val STKL_Y = UInt(0x100)
  val VASIZE = UInt(0x10000) // 2**16
  val VAMASK: Int = VASIZE - 1 // 2**16 - 1
  val MEMSIZE64K = UInt(0x10000) //  2**16
  val UNIMEMSIZE = UInt(0x40000) // 2**18
  val UNIMASK: Int = UNIMEMSIZE - 1 // 2**18 - 1
  val IOPAGEBASE = UInt(0x3fe000) // 2**22 - 2**13
  val IOPAGESIZE = UInt(0x2000) // 2**13
  val IOPAGEMASK: Int = IOPAGESIZE - 1 // 2**13 - 1
  val MAXMEMSIZE = UInt(0x400000) // 2**22
  val PAMASK: Int = MAXMEMSIZE - 1 // 2**22 - 1
  val DMASK = UInt(0xffff)
  val BMASK = UInt(0xff)

  /* PSW */

  val PSW_V_C = 0
  /* condition codes */
  val PSW_V_V = 1
  val PSW_V_Z = 2
  val PSW_V_N = 3
  val PSW_V_TBIT = 4 // trace trap
  val PSW_V_IPL = 5 // int priority
  val PSW_V_FPD = 8 // first part done
  val PSW_V_RS = 11 // register set
  val PSW_V_PM = 12 // previous mode
  val PSW_V_CM = 14 // current mode
  val PSW_CC = 0xf
  val PSW_TBIT: Int = 1 << PSW_V_TBIT
  val PSW_PM: Int = 3 << PSW_V_PM

  // PSW Bits
  val BIT_C: Int = 2 ^ 0 // Carry
  val BIT_V: Int = 2 ^ 1 // Overflow
  val BIT_Z: Int = 2 ^ 2 // Zero
  val BIT_N: Int = 2 ^ 3 // Negative
  val BIT_TBIT: Int = 2 ^ 4 // Trace Trap
  val BIT_IPL: Int = 2 ^ 5 // IPL - 3 bits
  val BIT_FPD: Int = 2 ^ 8 // First Part Done
  val BIT_NCF: Int = 2 ^ 9 // Must Be Zero - 2 bits
  val BIT_RS: Int = 2 ^ 11 // Register Set
  val BIT_PM: Int = 2 ^ 12 // Previous access mode, 2 bits
  val BIT_CM: Int = 2 ^ 14 // Current access mode, 2 bits

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

  val trap_vec = Array(/* trap req to vector */
    VEC_RED, VEC_ODD, VEC_MME, VEC_NXM,
    VEC_PAR, VEC_PRV, VEC_ILL, VEC_BPT,
    VEC_IOT, VEC_EMT, VEC_TRAP, VEC_TRC,
    VEC_YEL, VEC_PWRFL, VEC_FPE
  )

  /* Trap masks, descending priority order, following J-11
   An interrupt summary bit is kept with traps, to minimize overhead
*/

  val TRAP_V_RED = 0 //red stk abort  4
  val TRAP_V_ODD = 1 //odd address    4
  val TRAP_V_MME = 2 //mem mgt      250
  val TRAP_V_NXM = 3 //nx memory      4
  val TRAP_V_PAR = 4 //parity err   114
  val TRAP_V_PRV = 5 //priv inst      4
  val TRAP_V_ILL = 6 //illegal inst  10
  val TRAP_V_BPT = 7 //BPT           14
  val TRAP_V_IOT = 8 //IOT           20
  val TRAP_V_EMT = 9 //EMT           30
  val TRAP_V_TRAP = 10 // TRAP          34
  val TRAP_V_TRC = 11 //T bit         14
  val TRAP_V_YEL = 12 //stack          4
  val TRAP_V_PWRFL = 13 // power fail    24
  val TRAP_V_FPE = 14 // fpe          244
  val TRAP_V_MAX = 15 // intr = max trp #
  val ABRT_V_BKPT = 16 // stop due to breakpt
  val TRAP_RED: UInt = UInt(1) << TRAP_V_RED
  val TRAP_ODD: UInt = UInt(1) << TRAP_V_ODD
  val TRAP_MME: UInt = UInt(1) << TRAP_V_MME
  val TRAP_NXM: UInt = UInt(1) << TRAP_V_NXM
  val TRAP_PAR: UInt = UInt(1) << TRAP_V_PAR
  val TRAP_PRV: UInt = UInt(1) << TRAP_V_PRV
  val TRAP_ILL: UInt = UInt(1) << TRAP_V_ILL
  val TRAP_BPT: UInt = UInt(1) << TRAP_V_BPT
  val TRAP_IOT: UInt = UInt(1) << TRAP_V_IOT
  val TRAP_EMT: UInt = UInt(1) << TRAP_V_EMT
  val TRAP_TRAP: UInt = UInt(1) << TRAP_V_TRAP
  val TRAP_TRC: UInt = UInt(1) << TRAP_V_TRC
  val TRAP_YEL: UInt = UInt(1) << TRAP_V_YEL
  val TRAP_PWRFL: UInt = UInt(1) << TRAP_V_PWRFL
  val TRAP_FPE: UInt = UInt(1) << TRAP_V_FPE
  val TRAP_INT: UInt = UInt(1) << TRAP_V_MAX
  val TRAP_ALL: UInt = UInt((UInt(1) << TRAP_V_MAX) - 1)
  /* all traps */
  val ABRT_BKPT: UInt = UInt(1) << ABRT_V_BKPT


  val trap_clear = Array(/* trap clears */
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
  /* CPUERR */

  val CPUE_RED = UInt(0x4) // red stack
  val CPUE_YEL = UInt(0x8) // yellow stack
  val CPUE_TMO = UInt(0x10) // IO page nxm
  val CPUE_NXM = UInt(0x20) // memory nxm
  val CPUE_ODD = UInt(0x40) // odd address
  val CPUE_HALT = UInt(0x80) // HALT not kernel
  val CPUE_IMP = UInt(0xfc) // implemented bits

  /* Unibus I/O page layout - see pdp11_io_lib.c for address layout details
    Massbus devices (RP, TU) do not appear in the Unibus IO page */

  val IOBA_AUTO = 0 /* Assigned by Auto Configure */

  /* Processor registers which have I/O page addresses
   */

  val IOBA_CTL:Int = IOPAGEBASE + 0x1f50
  /* board ctrl */
  val IOLN_CTL = 0x8

  val IOBA_UCA:Int = IOPAGEBASE + 0xff8
  /* UC15 DR11 #1 */
  val IOLN_UCA = 0x6
  val IOBA_UCB:Int  = IOPAGEBASE + 0xff0
  /* UC15 DR11 #2 */
  val IOLN_UCB = 0x6
  val IOBA_UBM:Int  = IOPAGEBASE + 0x1080
  /* Unibus map */
  //val IOLN_UBM     =   (UBM_LNT_LW * sizeof (int32))
  val IOBA_MMR3:Int  = IOPAGEBASE + 0x154e
  /* MMR3 */
  val IOLN_MMR3 = 0x2
  val IOBA_TTI :Int = IOPAGEBASE + 0x1f70
  /* DL11 rcv */
  val IOLN_TTI = 0x4
  val IOBA_TTO :Int = IOPAGEBASE + 0x1f74
  /* DL11 xmt */
  val IOLN_TTO = 0x4
  val IOBA_SR :Int = IOPAGEBASE + 0x1f78
  /* SR */
  val IOLN_SR = 0x2
  val IOBA_MMR012:Int  = IOPAGEBASE + 0x1f7a
  /* MMR0-2 */
  val IOLN_MMR012 = 0x6
  val IOBA_GPR:Int  = IOPAGEBASE + 0x1fc0
  /* GPR's */
  val IOLN_GPR = 0x8
  val IOBA_UCTL:Int  = IOPAGEBASE + 0x1fd8
  /* UBA ctrl */
  val IOLN_UCTL = 0x8
  val IOBA_CPU: Int = IOPAGEBASE + 0x1fe0
  /* CPU reg */
  val IOLN_CPU = 0x1e
  val IOBA_PSW:Int = IOPAGEBASE + 0x1ffe
  /* PSW */
  val IOLN_PSW = 0x2
  val IOBA_UIPDR:Int = IOPAGEBASE + 0x1f80
  /* user APR's */
  val IOLN_UIPDR = 0x10
  val IOBA_UDPDR:Int = IOPAGEBASE + 0x1f90
  val IOLN_UDPDR = 0x10
  val IOBA_UIPAR:Int = IOPAGEBASE + 0x1fa0
  val IOLN_UIPAR = 0x10
  val IOBA_UDPAR:Int = IOPAGEBASE + 0x1fb0
  val IOLN_UDPAR = 0x10
  val IOBA_SUP:Int  = IOPAGEBASE + 0x1480
  /* supervisor APR's */
  val IOLN_SUP = 0x40
  val IOBA_KIPDR:Int = IOPAGEBASE + 0x14c0
  /* kernel APR's */
  val IOLN_KIPDR = 0x10
  val IOBA_KDPDR:Int = IOPAGEBASE + 0x14d0
  val IOLN_KDPDR = 0x10
  val IOBA_KIPAR:Int  = IOPAGEBASE + 0x14e0
  val IOLN_KIPAR = 0x10
  val IOBA_KDPAR:Int  = IOPAGEBASE + 0x14f0
  val IOLN_KDPAR = 0x10

  /* Interrupt assignments; within each level, priority is right to left
     PIRQn has the highest priority with a level and is always bit <0>
     On level 6, the clock is second highest priority */

  val IPL_HLVL = 8
  /* # int levels */
  val IPL_HMIN = 4 /* lowest IO int level */

  val INT_V_PIR7 = 0
  /* BR7 */
  val INT_V_UCA = 1

  val INT_V_PIR6 = 0
  /* BR6 */
  val INT_V_CLK = 1
  val INT_V_PCLK = 2
  val INT_V_DTA = 3
  val INT_V_TA = 4
  val INT_V_CR = 5 /* CR11 */

  val INT_V_PIR5 = 0
  /* BR5 */
  val INT_V_RK = 1
  val INT_V_RL = 2
  val INT_V_RX = 3
  val INT_V_TM = 4
  val INT_V_RP = 5
  val INT_V_TS = 6
  val INT_V_HK = 7
  val INT_V_RQ = 8
  val INT_V_DZRX = 9
  val INT_V_DZTX = 10
  val INT_V_TQ = 11
  val INT_V_RY = 12
  val INT_V_XQ = 13
  val INT_V_XU = 14
  val INT_V_TU = 15
  val INT_V_RF = 16
  val INT_V_RC = 17
  val INT_V_RS = 18
  val INT_V_DMCRX = 19
  val INT_V_DMCTX = 20
  val INT_V_DUPRX = 21
  val INT_V_DUPTX = 22
  val INT_V_KMCA = 23
  val INT_V_KMCB = 24
  val INT_V_UCB = 25
  val INT_V_CH = 26
  val INT_V_NG = 27

  val INT_V_PIR4 = 0
  /* BR4 */
  val INT_V_TTI = 1
  val INT_V_TTO = 2
  val INT_V_PTR = 3
  val INT_V_PTP = 4
  val INT_V_LPT = 5
  val INT_V_VHRX = 6
  val INT_V_VHTX = 7
  val INT_V_CD = 8
  /* CD11 */
  val INT_V_DLI = 9
  val INT_V_DLO = 10
  val INT_V_DCI = 11
  val INT_V_DCO = 12
  /* VT simulation is sequential, so only
     one interrupt is posted at a time. */
  val INT_V_VTST = 13
  val INT_V_VTLP = 14
  val INT_V_VTCH = 15
  val INT_V_VTNM = 16
  val INT_V_LK = 17
  val INT_V_TDRX = 18
  val INT_V_TDTX = 19

  val INT_V_PIR3 = 0
  /* BR3 */
  val INT_V_PIR2 = 0
  /* BR2 */
  val INT_V_PIR1 = 0 /* BR1 */

  val INT_PIR7: UInt = UInt(1) << INT_V_PIR7
  val INT_UCB: UInt = UInt(1) << INT_V_UCB
  val INT_PIR6: UInt = UInt(1) << INT_V_PIR6
  val INT_CLK: UInt = UInt(1) << INT_V_CLK
  val INT_PCLK: UInt = UInt(1) << INT_V_PCLK
  val INT_DTA: UInt = UInt(1) << INT_V_DTA
  val INT_TA: UInt = UInt(1) << INT_V_TA
  val INT_CR: UInt = UInt(1) << INT_V_CR
  val INT_PIR5: UInt = UInt(1) << INT_V_PIR5
  val INT_RK: UInt = UInt(1) << INT_V_RK
  val INT_RL: UInt = UInt(1) << INT_V_RL
  val INT_RX: UInt = UInt(1) << INT_V_RX
  val INT_TM: UInt = UInt(1) << INT_V_TM
  val INT_RP: UInt = UInt(1) << INT_V_RP
  val INT_TS: UInt = UInt(1) << INT_V_TS
  val INT_HK: UInt = UInt(1) << INT_V_HK
  val INT_RQ: UInt = UInt(1) << INT_V_RQ
  val INT_DZRX: UInt = UInt(1) << INT_V_DZRX
  val INT_DZTX: UInt = UInt(1) << INT_V_DZTX
  val INT_TQ: UInt = UInt(1) << INT_V_TQ
  val INT_RY: UInt = UInt(1) << INT_V_RY
  val INT_XQ: UInt = UInt(1) << INT_V_XQ
  val INT_XU: UInt = UInt(1) << INT_V_XU
  val INT_TU: UInt = UInt(1) << INT_V_TU
  val INT_RF: UInt = UInt(1) << INT_V_RF
  val INT_RC: UInt = UInt(1) << INT_V_RC
  val INT_RS: UInt = UInt(1) << INT_V_RS
  val INT_DMCRX: UInt = UInt(1) << INT_V_DMCRX
  val INT_DMCTX: UInt = UInt(1) << INT_V_DMCTX
  val INT_KMCA: UInt = UInt(1) << INT_V_KMCA
  val INT_KMCB: UInt = UInt(1) << INT_V_KMCB
  val INT_DUPRX: UInt = UInt(1) << INT_V_DUPRX
  val INT_DUPTX: UInt = UInt(1) << INT_V_DUPTX
  val INT_UCA: UInt = UInt(1) << INT_V_UCA
  val INT_PIR4: UInt = UInt(1) << INT_V_PIR4
  val INT_TTI: UInt = UInt(1) << INT_V_TTI
  val INT_TTO: UInt = UInt(1) << INT_V_TTO
  val INT_PTR: UInt = UInt(1) << INT_V_PTR
  val INT_PTP: UInt = UInt(1) << INT_V_PTP
  val INT_LPT: UInt = UInt(1) << INT_V_LPT
  val INT_VHRX: UInt = UInt(1) << INT_V_VHRX
  val INT_VHTX: UInt = UInt(1) << INT_V_VHTX
  val INT_CD: UInt = UInt(1) << INT_V_CD
  val INT_DLI: UInt = UInt(1) << INT_V_DLI
  val INT_DLO: UInt = UInt(1) << INT_V_DLO
  val INT_DCI: UInt = UInt(1) << INT_V_DCI
  val INT_DCO: UInt = UInt(1) << INT_V_DCO
  val INT_VTLP: UInt = UInt(1) << INT_V_VTLP
  val INT_VTST: UInt = UInt(1) << INT_V_VTST
  val INT_VTCH: UInt = UInt(1) << INT_V_VTCH
  val INT_VTNM: UInt = UInt(1) << INT_V_VTNM
  val INT_LK: UInt = UInt(1) << INT_V_LK
  val INT_PIR3: UInt = UInt(1) << INT_V_PIR3
  val INT_PIR2: UInt = UInt(1) << INT_V_PIR2
  val INT_PIR1: UInt = UInt(1) << INT_V_PIR1
  val INT_TDRX: UInt = UInt(1) << INT_V_TDRX
  val INT_TDTX: UInt = UInt(1) << INT_V_TDTX
  val INT_CH: UInt = UInt(1) << INT_V_CH
  val INT_NG: UInt = UInt(1) << INT_V_NG

  val INT_INTERNAL7: UInt = INT_PIR7
  val INT_INTERNAL6: UInt = INT_PIR6 | INT_CLK
  val INT_INTERNAL5: UInt = INT_PIR5
  val INT_INTERNAL4: UInt = INT_PIR4
  val INT_INTERNAL3: UInt = INT_PIR3
  val INT_INTERNAL2: UInt = INT_PIR2
  val INT_INTERNAL1: UInt = INT_PIR1

  val IPL_UCB = 7
  /* int pri levels */
  val IPL_CLK = 6
  val IPL_PCLK = 6
  val IPL_DTA = 6
  val IPL_TA = 6
  val IPL_CR = 6
  val IPL_RK = 5
  val IPL_RL = 5
  val IPL_RX = 5
  val IPL_TM = 5
  val IPL_RP = 5
  val IPL_TS = 5
  val IPL_HK = 5
  val IPL_RQ = 5
  val IPL_DZRX = 5
  val IPL_DZTX = 5
  val IPL_TQ = 5
  val IPL_RY = 5
  val IPL_XQ = 5
  val IPL_XU = 5
  val IPL_CH = 5
  val IPL_TU = 5
  val IPL_RF = 5
  val IPL_RC = 5
  val IPL_RS = 5
  val IPL_DMCRX = 5
  val IPL_DMCTX = 5
  val IPL_KMCA = 5
  val IPL_KMCB = 5
  val IPL_DUPRX = 5
  val IPL_DUPTX = 5
  val IPL_UCA = 5
  val IPL_NG = 5
  val IPL_PTR = 4
  val IPL_PTP = 4
  val IPL_TTI = 4
  val IPL_TTO = 4
  val IPL_LPT = 4
  val IPL_VHRX = 4
  val IPL_VHTX = 4
  val IPL_CD = 4
  val IPL_DLI = 4
  val IPL_DLO = 4
  val IPL_DCI = 4
  val IPL_DCO = 4
  val IPL_VTLP = 4
  val IPL_VTST = 4
  val IPL_VTCH = 4
  val IPL_VTNM = 4
  val IPL_LK = 4
  /* XXX just a guess */
  val IPL_TDRX = 4
  val IPL_TDTX = 4

  val IPL_PIR7 = 7
  val IPL_PIR6 = 6
  val IPL_PIR5 = 5
  val IPL_PIR4 = 4
  val IPL_PIR3 = 3
  val IPL_PIR2 = 2
  val IPL_PIR1 = 1

  /* Device vectors */

  val VEC_AUTO = 0
  /* Assigned by Auto Configure */
  val VEC_FLOAT = 0 /* Assigned by Auto Configure */

  /* Processor specific internal fixed vectors */
  val VEC_PIRQ = 0xa0
  val VEC_TTI = 0x30
  val VEC_TTO = 0x34
  val VEC_UCA = 0xc0
  val VEC_UCB = 0xc8

}
