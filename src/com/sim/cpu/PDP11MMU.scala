package com.sim.cpu

import com.sim.unsigned.{UInt, UShort}
import scala.annotation.switch

class PDP11MMU(cpu: PDP11) extends BasicMMU(cpu) {


  // Registers
  val MMR0 = new Register16("MMR0") // MMR0 - Status
  val MMR1 = new Register16("MMR1") // MMR1 - R+/-R
  val MMR2 = new Register16("MMR2") // MMR2 - saved PC
  val MMR3 = new Register16("MMR3") // MMR3 - 22b status

  val KIPAR0 = new Register16("KIPARO")
  val KIPDR0 = new Register16("KIPDRO")
  val KIP0 = new CompositeRegister32("KIP0", KIPAR0, KIPDR0)
  val MMR0_FREEZE = 0xe000

  /* if set, no update */
  @inline def update_MM: Boolean = ((MMR0 & MMR0_FREEZE) == 0)

  MMR0(0)
  MMR1(0)
  MMR2(0)
  MMR3(0)
  KIP0(0)

  /* PARs/PDRs */
  val APRFILE: Array[UInt] = new Array[UInt](64)

  var isenable: Int = 0
  var dsenable: Int = 0
  /* i, d space flags */
  var reg_mods: Int = 0
  /* reg deltas */
  var last_pa: UInt = null /* pa from ReadMW/ReadMB */

  def calc_MMR1(val1: Int) = (if (reg_mods != 0) (((val1) << 8) | reg_mods) else (val1))

  /* Virtual address */

  val VA_DF = 0x1fff
  /* displacement */
  val VA_BN = 0x1fc0
  /* block number */
  val VA_V_APF = 13
  /* offset to APF */
  val VA_V_DS = 16
  /* offset to space */
  val VA_V_MODE = 17
  /* offset to mode */
  val VA_DS = (UInt(1) << VA_V_DS) /* data space flag */

  /* I/O access modes */

  val READ = 0
  val READC = 1
  /* read console */
  val WRITE = 2
  val WRITEC = 3
  /* write console */
  val WRITEB = 4

  /* PDR */

  val PDR_ACF = 0x7
  /* access control */
  val PDR_ACS = 0x6
  /* 2b access control */
  val PDR_ED = 0x8
  /* expansion dir */
  val PDR_W = 0x40
  /* written flag */
  val PDR_A = 0x80
  /* access flag */
  val PDR_PLF = 0x7f00
  /* page lnt field */
  val PDR_NOC = 0x8000 /* don't cache */

  val PDR_PRD = 0x3 /* page readable if 2 */

  /* MMR0 */

  val MMR0_MME    =    0x1                         /* mem mgt enable */
  val MMR0_V_PAGE =    1                               /* offset to pageno */
  val MMR0_M_PAGE =    0x3f                             /* mask for pageno */
  val MMR0_PAGE   =    (MMR0_M_PAGE << MMR0_V_PAGE)
  val MMR0_IC     =    0x80                         /* instr complete */
  val MMR0_MAINT  =    0x100                         /* maintenance */
  val MMR0_TENB   =    0x200                         /* trap enable */
  val MMR0_TRAP   =    0x1000                         /* mem mgt trap */
  val MMR0_RO     =    0x2000                         /* read only error */
  val MMR0_PL     =    0x4000                         /* page lnt error */
  val MMR0_NR     =    0x8000                         /* no access error */
  val MMR0_FREEZE =    0xe000                         /* if set, no update */
  val MMR0_WR     =    0xf301                         /* writeable bits */

  /* MMR3 */

  val MMR3_UDS     =   0x1                             /* user dspace enbl */
  val MMR3_SDS     =   0x2                             /* super dspace enbl */
  val MMR3_KDS     =   0x4                             /* krnl dspace enbl */
  val MMR3_CSM     =   0x8                             /* CSM enable */
  val MMR3_M22E    =   0x10                             /* 22b mem mgt enbl */
  val MMR3_BME     =   0x20                             /* DMA bus map enbl */
  
  /* Effective address calculations
     Inputs:
          spec    =       specifier <5:0>
     Outputs:
          ea      =       effective address
                          <15:0> =  virtual address
                          <16> =    instruction/data data space
                          <18:17> = mode
     Data space calculation: the PDP-11 features both instruction and data
     spaces.  Instruction space contains the instruction and any sequential
     add ons (eg, immediates, absolute addresses).  Data space contains all
     data operands and indirect addresses.  If data space is enabled, then
     memory references are directed according to these rules:
          Mode    Index ref       Indirect ref            Direct ref
          10..16  na              na                      data
          17      na              na                      instruction
          20..26  na              na                      data
          27      na              na                      instruction
          30..36  na              data                    data
          37      na              instruction (absolute)  data
          40..46  na              na                      data
          47      na              na                      instruction
          50..56  na              data                    data
          57      na              instruction             data
          60..67  instruction     na                      data
          70..77  instruction     data                    data
     According to the PDP-11 Architecture Handbook, MMR1 records all
     autoincrement and autodecrement operations, including those which
     explicitly reference the PC.  For the J-11, this is only true for
     autodecrement operands, autodecrement deferred operands, and
     autoincrement destination operands that involve a write to memory.
     The simulator follows the Handbook, for simplicity.
     Notes:
     - dsenable will direct a reference to data space if data space is enabled
     - ds will direct a reference to data space if data space is enabled AND if
          the specifier register is not PC; this is used for 17, 27, 37, 47, 57
     - Modes 2x, 3x, 4x, and 5x must update MMR1 if updating enabled
     - Modes 46 and 56 must check for stack overflow if kernel mode
  */

  /* Effective address calculation for words */

  def GeteaW(spec: UInt): UInt = {
    var adr: UInt = null

    var reg = spec & 0x7
    /* register number */
    var ds = if (reg == 7) isenable else dsenable /* dspace if not PC */

    ((spec >> 3): @switch) match {
      /* decode spec<5:3> */

      case 1 => /* (R) */
        return UInt(cpu.R(reg) | ds)

      case 2 => /* (R)+ */
        adr = UInt((cpu.R(reg) + UInt(2)) & 0xffff)
        (cpu.R(reg)).set16(adr.intValue)
        //cpu.R(reg) (((adr = cpu.R(reg).toUInt) + UInt(2) )& 0xffff)
        reg_mods = calc_MMR1(0x10 | reg);
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        return (adr | UInt(ds))

      case 3 => /* @(R)+ */
        adr = UInt(cpu.R(reg) + UInt(2) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0x10 | reg);
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        adr = ReadW(adr | UInt(ds))
        return (adr | UInt(dsenable))

      case 4 => /* -(R) */
        //adr = R[reg] = (R[reg] - 2) & 0xffff;
        adr = UInt((cpu.R(reg) - UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0xf0 | reg);
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr.intValue < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr);
        return UInt(adr | ds)

      case 5 => /* @-(R) */
        //adr = R[reg] = (R[reg] - 2) & 0xffff;
        adr = UInt((cpu.R(reg) - UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0xf0 | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods);
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr.intValue < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr);
        adr = ReadW(adr | ds)
        return (adr | dsenable)

      case 6 => /* d(r) */
        adr = ReadW(cpu.PC | isenable)
        cpu.PC((cpu.PC + 2) & 0xffff)
        return (((cpu.R(reg) + adr) & 0xffff) | dsenable)

      case 7 => /* @d(R) */
        adr = ReadW(cpu.PC | isenable)
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        adr = ReadW(((cpu.R(reg) + adr) & 0xffff) | dsenable)
        return (adr | dsenable)
    } /* end switch */
  }

  /* Effective address calculation for bytes */

  def GeteaB(spec: UInt): UInt = {
    val reg = spec & 0x7
    /* reg number */
    val ds = if (reg == 7) isenable else dsenable /* dspace if not PC */
    (spec >> 3: @switch) match {
      /* decode spec<5:3> */

      case 1 => /* (R) */
        return UInt(cpu.R(reg) | ds)

      case 2 => /* (R)+ */
        val delta = 1 + {
          if (reg >= 6) -1 else 0
        }
        /* 2 if R6, PC */
        val adr = UInt((cpu.R(reg) + delta) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1((delta << 3) | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods);
        return (adr | ds);

      /* @(R)+ */
      case 3 =>
        var adr: UInt = UInt((cpu.R(reg) + UInt(2)) & 0xffff)
        cpu.R(reg).set16(adr.intValue)
        reg_mods = calc_MMR1(0x10 | reg);
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        adr = ReadW(adr | ds);
        return (adr | dsenable);

      case 4 => /* -(R) */
        val delta = UInt(1) + (reg >= 6) /* 2 if R6, PC */
        cpu.R(reg).set16((cpu.R(reg) - delta) & 0xffff)

        val adr = cpu.R(reg)
        reg_mods = calc_MMR1((((-delta) & 0x1f) << 3) | reg)
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr)
        return (adr | ds)

      case 5 => /* @-(R) */
        cpu.R(reg).set16((cpu.R(reg) - 2) & 0xffff)
        val adr = cpu.R(reg)
        reg_mods = calc_MMR1(0xf0 | reg);
        if (update_MM && (reg != 7))
          MMR1.set16(reg_mods)
        if ((reg == 6) && (cpu.cm == cpu.MD_KER) && (adr < (cpu.STKLIM + PDP11.STKL_Y)))
          cpu.set_stack_trap(adr)
        adr = ReadW(adr | ds);
        return (adr | dsenable);

      case 6 => /* d(r) */
        val adr = ReadW(cpu.PC | isenable);
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        return (((cpu.R(reg) + adr) & 0xffff) | dsenable);

      case 7 => /* @d(R) */
        val adr = ReadW(cpu.PC | isenable);
        cpu.PC.set16((cpu.PC + 2) & 0xffff)
        adr = ReadW(((cpu.R(reg) + adr) & 0xffff) | dsenable);
        return (adr | dsenable);
    } /* end switch */
  }

  /* Read byte and word routines, read only and read-modify-write versions
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          data    =       data read from memory or I/O space
  */

  // TODO Solve the data write
  def ReadE(va: UInt): UInt = {

    if (((va & 1) && cpu.CPUT(CPUOPT.HAS_ODD)) != 0) {
      /* odd address? */
      setCPUERR(CPUE_ODD);
      ABORT(TRAP_ODD);
    }
    val pa = relocR(va); /* relocate */
    if (BPT_SUMM_RD &&
      (sim_brk_test(va & 0xffff, BPT_RDVIR) ||
        sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    if (ADDR_IS_MEM(pa)) /* memory address? */
      return RdMemW(pa);
    if ((pa < IOPAGEBASE) || /* not I/O address */
      (cpu.CPUT(CPUT_J) && (pa >= IOBA_CPU))) {
      /* or J11 int reg? */
      setCPUERR(CPUE_NXM);
      ABORT(TRAP_NXM);
    }
    if (iopageR(& data, pa, READ) != SCPE_OK) {
      /* invalid I/O addr? */
      setCPUERR(CPUE_TMO);
      ABORT(TRAP_NXM);
    }
    return data;
  }

  def ReadW(va: UInt): UInt = {


    if ((va & 1) && cpu.CPUT(cpu.HAS_ODD)) {
      /* odd address? */
      setCPUERR(cpu.CPUE_ODD);
      ABORT(cpu.TRAP_ODD);
    }
    val pa = relocR(va); /* relocate */
    if (BPT_SUMM_RD &&
      (cpu.sim_brk_test(va & 0xffff, BPT_RDVIR) ||
        cpu.sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    return PReadW(pa);
  }

  def ReadB(va: UInt): UInt = {

    val pa = relocR(va); /* relocate */
    if (BPT_SUMM_RD &&
      (cpu.sim_brk_test(va & 0xffff, BPT_RDVIR) ||
        cpu.sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    return PReadB(pa);
  }

  /* Read word with breakpoint check: if a data breakpoint is encountered,
     set reason accordingly but don't do an ABORT.  This is used when we want
     to break after doing the operation, used for interrupt processing.  */
  def ReadCW(va: UInt): UInt = {
    if ((va & 1) && CPUT(HAS_ODD)) {
      /* odd address? */
      setCPUERR(CPUE_ODD);
      ABORT(TRAP_ODD);
    }
    val pa = relocR(va); /* relocate */
    if (BPT_SUMM_RD &&
      (sim_brk_test(va & 0xffff, BPT_RDVIR) ||
        sim_brk_test(pa, BPT_RDPHY))) /* read breakpoint? */
      reason = STOP_IBKPT; /* report that */
    return PReadW(pa);
  }

  def ReadMW(va: UInt): UInt = {
    if ((va & 1) && CPUT(HAS_ODD)) {
      /* odd address? */
      setCPUERR(CPUE_ODD);
      ABORT(TRAP_ODD);
    }
    last_pa = relocW(va); /* reloc, wrt chk */
    if (BPT_SUMM_RW &&
      (sim_brk_test(va & 0xffff, BPT_RWVIR) ||
        sim_brk_test(last_pa, BPT_RWPHY))) /* read or write breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    return PReadW(last_pa);
  }

  def ReadMB(va: UInt): UInt = {
    last_pa = relocW(va); /* reloc, wrt chk */
    if (BPT_SUMM_RW &&
      (sim_brk_test(va & 0xffff, BPT_RWVIR) ||
        sim_brk_test(last_pa, BPT_RWPHY))) /* read or write breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    return PReadB(last_pa);
  }

  def PReadW(pa: UInt): UInt = {
    int32 data;

    if (ADDR_IS_MEM(pa)) /* memory address? */
      return RdMemW(pa);
    if (pa < cpu.IOPAGEBASE) {
      /* not I/O address? */
      setCPUERR(CPUE_NXM);
      ABORT(TRAP_NXM);
    }
    if (iopageR(& data, pa, READ) != SCPE_OK) {
      /* invalid I/O addr? */
      setCPUERR(CPUE_TMO);
      ABORT(TRAP_NXM);
    }
    return data;
  }

  def PReadB(pa: UInt): UInt = {
    int32 data;

    if (ADDR_IS_MEM(pa)) /* memory address? */
      return RdMemB(pa);
    if (pa < cpu.IOPAGEBASE) {
      /* not I/O address? */
      setCPUERR(CPUE_NXM);
      ABORT(TRAP_NXM);
    }
    if (iopageR(& data, pa, READ) != SCPE_OK) {
      /* invalid I/O addr? */
      setCPUERR(CPUE_TMO);
      ABORT(TRAP_NXM);
    }
    return ((pa & 1) ? data >> 8: data) & 0xff;
  }

  /* Write byte and word routines
     Inputs:
          data    =       data to be written
          va      =       virtual address, <18:16> = mode, I/D space, or
          pa      =       physical address
     Outputs: none
  */

  def WriteW(data: UInt, va: UInt): Unit = {
    if ((va & 1) && CPUT(HAS_ODD)) {
      /* odd address? */
      setCPUERR(CPUE_ODD);
      ABORT(TRAP_ODD);
    }
    val pa = relocW(va); /* relocate */
    if (BPT_SUMM_WR &&
      (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
        sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    PWriteW(data, pa);
  }

  def WriteB(data: UInt, va: UInt): Unit = {
    val pa = relocW(va) /* relocate */
    if (BPT_SUMM_WR &&
      (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
        sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
      ABORT(ABRT_BKPT); /* stop simulation */
    PWriteB(data, pa);
  }

  /* Write word with breakpoint check: if a data breakpoint is encountered,
     set reason accordingly but don't do an ABORT.  This is used when we want
     to break after doing the operation, used for interrupt processing.  */
  def WriteCW(data: UInt, va: UInt): Unit = {
    if ((((va & UInt(1)) != 0) && cpu.CPUT(CPUOPT.HAS_ODD) != 0) != 0) {
      /* odd address? */
      setCPUERR(CPUE_ODD);
      ABORT(TRAP_ODD);
    }
    val pa = relocW(va) /* relocate */
    if (BPT_SUMM_WR &&
      (sim_brk_test(va & 0xffff, BPT_WRVIR) ||
        sim_brk_test(pa, BPT_WRPHY))) /* write breakpoint? */
      reason = STOP_IBKPT; /* report that */
    PWriteW(data, pa);
  }

  def PWriteW(data: UInt, pa: UInt): Unit = {
    if (ADDR_IS_MEM(pa)) {
      /* memory address? */
      WrMemW(pa, data);
      return;
    }
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      setCPUERR(CPUE_NXM);
      ABORT(TRAP_NXM);
    }
    if (iopageW(data, pa, WRITE) != SCPE_OK) {
      /* invalid I/O addr? */
      setCPUERR(CPUE_TMO);
      ABORT(TRAP_NXM);
    }
    return;
  }

  def PWriteB(data: UInt, pa: UInt): Unit = {
    if (ADDR_IS_MEM(pa)) {
      /* memory address? */
      WrMemB(pa, data);
      return;
    }
    if (pa < PDP11.IOPAGEBASE) {
      /* not I/O address? */
      setCPUERR(CPUE_NXM);
      ABORT(TRAP_NXM);
    }
    if (iopageW(data, pa, WRITEB) != SCPE_OK) {
      /* invalid I/O addr? */
      setCPUERR(CPUE_TMO);
      ABORT(TRAP_NXM);
    }
    return;
  }

  /* Relocate virtual address, read access
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          pa      =       physical address
     On aborts, this routine aborts back to the top level simulator
     with an appropriate trap code.
     Notes:
     - The 'normal' read codes (010, 110) are done in-line; all
       others in a subroutine
     - APRFILE[UNUSED] is all zeroes, forcing non-resident abort
     - Aborts must update MMR0<15:13,6:1> if updating is enabled
  */

  def relocR(va: UInt): UInt = {
    var pa: UInt = UInt(0)

    if (MMR0 & MMR0_MME) {
      /* if mmgt */
        val apridx = (va >> VA_V_APF) & 0x3f
      /* index into APR */
      val apr = APRFILE(apridx) /* with va<18:13> */
      if ((apr & PDR_PRD) != 2) /* not 2, 6? */
        relocR_test(va, apridx); /* long test */
      if (PLF_test(va, apr)) /* pg lnt error? */
        reloc_abort(MMR0_PL, apridx);
      pa = ((va & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK;
      if ((MMR3 & MMR3_M22E) == 0) {
        pa = pa & UInt(0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000) | pa;
      }
    }
    else {
      pa = va & UInt(0xffff); /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000) | pa;
    }
    return pa;
  }

  /* Read relocation, access control field != read only or read/write
     ACF value            11/45,11/70             all others
     0                    abort NR                abort NR
     1                    trap                    -
     2                    ok                      ok
     3                    abort NR                -
     4                    trap                    abort NR
     5                    ok                      -
     6                    ok                      ok
     7                    abort NR                -
  */

  def relocR_test(va: UInt, apridx: Int): Unit = {
    var err = 0;
    /* init status */
    val apr = APRFILE(apridx); /* get APR */
    (apr & PDR_ACF) match {
      /* case on ACF */

      case (1 | 4) => /* trap read */
        if (cpu.CPUT(CPUOPT.HAS_MMTR)) {
          /* traps implemented? */
          APRFILE(apridx) = APRFILE(apridx) | PDR_A; /* set A */
          if (MMR0 & MMR0_TENB) {
            /* traps enabled? */
            if (update_MM) /* update MMR0 */
              MMR0 = (MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE);
            MMR0 = MMR0 | MMR0_TRAP; /* set trap flag */
            setTRAP(TRAP_MME); /* set trap */
          }
          return; /* continue op */
        } /* not impl, abort NR */
      case (0 | 3 | 7) => /* non-resident */
        err = MMR0_NR /* set MMR0 */
     /* go test PLF, abort */

      case (2 | 5 | 6) => /* readable */
        return; /* continue */
    } /* end switch */

    if (PLF_test(va, apr)) /* pg lnt error? */
      err = err | MMR0_PL;
    reloc_abort(err, apridx);
  }

  def PLF_test(va: UInt, apr: UInt): Boolean = {
    val dbn = va & VA_BN;
    /* extr block num */
    val plf = (apr & PDR_PLF) >> 2; /* extr page length */

    if ((apr.intValue & PDR_ED) != 0) (dbn < plf) else (dbn > plf) /* pg lnt error? */
  }

  def reloc_abort(err: UInt, apridx: UInt): Unit = {
    if (update_MM) MMR0 = /* update MMR0 */
      (MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE);
    APRFILE(apridx) = APRFILE(apridx) | PDR_A; /* set A */
    MMR0 = MMR0 | err; /* set aborts */
    ABORT(TRAP_MME); /* abort ref */
  }

  /* Relocate virtual address, write access
     Inputs:
          va      =       virtual address, <18:16> = mode, I/D space
     Outputs:
          pa      =       physical address
     On aborts, this routine aborts back to the top level simulator
     with an appropriate trap code.
     Notes:
     - The 'normal' write code (110) is done in-line; all others
       in a subroutine
     - APRFILE[UNUSED] is all zeroes, forcing non-resident abort
     - Aborts must update MMR0<15:13,6:1> if updating is enabled
  */

  def relocW(va: UInt): UInt = {
    var pa: UInt = null

    if (MMR0 & MMR0_MME) {
      /* if mmgt */
        val apridx = (va >> VA_V_APF) & 0x3f
      /* index into APR */
      val apr = APRFILE(apridx); /* with va<18:13> */
      if ((apr & PDR_ACF) != 6) /* not writeable? */
        relocW_test(va, apridx) /* long test */
      if (PLF_test(va, apr)) /* pg lnt error? */
        reloc_abort(MMR0_PL, apridx);
      APRFILE(apridx) = apr | PDR_W; /* set W */
      pa = UInt(((va & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK)
      if ((MMR3 & MMR3_M22E) == 0) {
        pa = UInt(pa & 0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000 | pa)
      }
    }
    else {
      pa = UInt(va & 0xffff) /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000 | pa)
    }
    return pa;
  }

  /* Write relocation, access control field != read/write
     ACF value            11/45,11/70             all others
     0                    abort NR                abort NR
     1                    abort RO                -
     2                    abort RO                abort RO
     3                    abort NR                -
     4                    trap                    abort NR
     5                    trap                    -
     6                    ok                      ok
     7                    abort NR                -
  */

  def relocW_test(va: UInt, apridx: Int): UInt = {

    var err = 0;
    /* init status */
    val apr = APRFILE(apridx); /* get APR */
    (apr & PDR_ACF: @switch) match {
      /* case on ACF */

      case (4 | 5) => /* trap write */
        if (cpu.CPUT(CPUOPT.HAS_MMTR)) {
          /* traps implemented? */
          APRFILE(apridx) = APRFILE(apridx) | PDR_A; /* set A */
          if (MMR0 & MMR0_TENB) {
            /* traps enabled? */
            if (update_MM) /* update MMR0 */
              MMR0 = (MMR0 & ~MMR0_PAGE) | (apridx << MMR0_V_PAGE);
            MMR0 = MMR0 | MMR0_TRAP; /* set trap flag */
            setTRAP(TRAP_MME); /* set trap */
          }
          return; /* continue op */
        } /* not impl, abort NR */
      case (0 | 3 | 7) => /* non-resident */
        err = MMR0_NR; /* MMR0 status */

      case (1 | 2) => /* read only */
        err = MMR0_RO; /* MMR0 status */


      case 6 => /* read/write */
        return; /* continue */
    } /* end switch */
    if (PLF_test(va, apr)) /* pg lnt error? */
      err = err | MMR0_PL;
    reloc_abort(err, apridx);
    return;
  }

  /* Relocate virtual address, console access
     Inputs:
          va      =       virtual address
          sw      =       switches
     Outputs:
          pa      =       physical address
     On aborts, this routine returns MAXMEMSIZE
  */

  def relocC(va: UInt, sw: UInt): UInt = {
    var mode: Int = 0
    var pa: UInt = UInt(0)

    if (MMR0 & MMR0_MME) {
      /* if mmgt */
      if (sw & SWMASK('K'))
        mode = cpu.MD_KER
      else if (sw & SWMASK('S'))
        mode = cpu.MD_SUP
      else if (sw & SWMASK('U'))
        mode = cpu.MD_USR
      else if (sw & SWMASK('P'))
        mode = (PSW >> PSW_V_PM) & 0x3;
      else mode = (PSW >> PSW_V_CM) & 0x3;
      va = va | ((sw & SWMASK('T')) ? cpu.calc_ds(mode): cpu.calc_is (mode)
      );
      val apridx = (va >> VA_V_APF) & 0x3f;
      /* index into APR */
      val apr = APRFILE(apridx)
      /* with va<18:13> */
      val dbn = va & VA_BN;
      /* extr block num */
      val plf = (apr & PDR_PLF) >> 2; /* extr page length */
      if ((apr & PDR_PRD) == 0) /* not readable? */
        return PDP11.MAXMEMSIZE
      if ((apr & PDR_ED) ? dbn < plf: dbn > plf)
        return PDP11.MAXMEMSIZE;
      pa = ((va & VA_DF) + ((apr >> 10) & 0x3fffc0)) & PDP11.PAMASK;
      if ((MMR3 & MMR3_M22E) == 0) {
        pa = pa & UInt(0x3ffff)
        if (pa >= UInt(0x3e000))
          pa = UInt(0x3c0000 | pa)
      }
    }
    else {
      pa = va & UInt(0xffff) /* mmgt off */
      if (pa >= UInt(0xe000))
        pa = UInt(0x3f0000 | pa)
    }
    return pa;
  }

  /* Memory management registers
     MMR0 17777572        read/write, certain bits unimplemented or read only
     MMR1 17777574        read only
     MMR2 17777576        read only
     MMR3 17772516        read/write, certain bits unimplemented
  */

  // TODO Solve data return
  def MMR012_rd(pa: UInt, access: UInt): Option[UInt] = {
    ((pa >> 1) & 3) match {
      /* decode pa<2:1> */

      case 0 => /* SR */
        None

      case 1 => /* MMR0 */
        //data(MMR0 & cpu_tab[cpu_model].mm0)
        Some(MMR0.get16 & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm0)

      case 2 =>
        /* MMR1 */

        Some(MMR1.get16)


      case 3 => /* MMR2 */
        Some(MMR2.get16)
    } /* end switch pa */
  }

  // TODO Solve data return issue
  def MMR012_wr(data: UInt, pa: UInt, access: Int): UInt = {
    ((pa >> 1) & 3) match {
      /* decode pa<2:1> */

      case 0 => /* DR */


      case 1 => /* MMR0 */
        if (access == WRITEB)
          data = (pa & 1) ? (MMR0 & 0xff) | (data << 8): (MMR0 & ~0xff) | data;
    data = data & cpu_tab[cpu_model].mm0;
    MMR0 = (MMR0 & ~ MMR0_WR) | (data & MMR0_WR);


      case _ => /* MMR1, MMR2 */

    }
    /* end switch pa */
    data
  }

  // TODO solve data return
  def MMR3_rd(): UInt = /* MMR3 */ {
    MMR3 & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm3;
  }

  def MMR3_wr(data: UInt, pa: UInt, access: UInt): Unit = /* MMR3 */ {
    if ((pa & 1) != 0) return

    MMR3 = data & CPUOPT.cpu_tab(cpu.cpu_model.intValue).mm3
    cpu.cpu_bme = (MMR3 & MMR3_BME) && (cpu.cpu_opt & CPUOPT.OPT_UBM)
    dsenable = cpu.calc_ds(cpu.cm)

  }

  /* PARs and PDRs.  These are grouped in I/O space as follows:
          17772200 - 17772276     supervisor block
          17772300 - 17772376     kernel block
          17777600 - 17777676     user block
     Within each block, the subblocks are I PDR's, D PDR's, I PAR's, D PAR's
     Thus, the algorithm for converting between I/O space addresses and
     APRFILE indices is as follows:
          idx<3:0> =      dspace'page     =       pa<4:1>
          par     =       PDR vs PAR      =       pa<5>
          idx<5:4> =      ker/sup/user    =       pa<8>'~pa<6>
     Note: the A,W bits are read only; they are cleared by any write to an APR
  */

  def APR_rd(pa: UInt, access: UInt): UInt = {
    var idx = (pa >> 1) & 0xf
    /* dspace'page */
    var left = (pa >> 5) & 1 /* PDR vs PAR */
    if ((pa & 0x40) == 0) /* 1 for super, user */
      idx = idx | 0x10;
    if ((pa & 0x100) != 0) /* 1 for user only */
      idx = idx | 0x20;
    if (left != 0)
      (APRFILE(idx) >> 16) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par
    else APRFILE(idx) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr
  }

  // TODO Solve setting data
  def APR_wr(data: UInt, pa: UInt, access: Int): UInt = {


    var idx = (pa >> 1) & 0xf
    /* dspace'page */
    var left = (pa >> 5) & 1 /* PDR vs PAR */
    if ((pa & 0x40) == 0) /* 1 for super, user */
      idx = idx | 0x10
    if ((pa & 0x100) != 0) /* 1 for user only */
      idx = idx | 0x20
    val curr = {
      if (left != 0)
        (APRFILE(idx) >> 16) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par
      else APRFILE(idx) & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr
    }
    if (access == WRITEB)
      data = if ((pa & 1) != 0) (curr & 0xff) | (data << 8) else (curr & ~0xff) | data
    APRFILE(idx) = {
      if (left != 0)
        ((APRFILE(idx) & 0xffff) |
          (((data & CPUOPT.cpu_tab(cpu.cpu_model.intValue).par)) << 16)) & ~(PDR_A | PDR_W)
      else ((APRFILE(idx) & ~0xffff) |
        (data & CPUOPT.cpu_tab(cpu.cpu_model.intValue).pdr)) & ~(PDR_A | PDR_W)
    }
  }


}
