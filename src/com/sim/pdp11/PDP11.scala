package com.sim.pdp11

import com.sim.SimTimer
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

  override def runcpu(singleStep: Boolean): Unit = {


    var IR = UInt(0)
    var srcspec = 0
    var srcreg = true
    var dstspec = 0
    var dstreg = true
    var src = 0
    var src2 = 0
    var dst = 0
    var ea = 0
    var i = 0
    var t = 0
    var sign = 0
    var oldrs = 0

    // TODO Bunch of housekeeping

    IR = MMU.ReadE(PC | UInt(MMU.isenable)) /* fetch instruction */
    SimTimer.sim_interval = SimTimer.sim_interval - 1
    srcspec = (IR >> 6) & 0x3f /* src, dst specs */
    dstspec = IR & 0x3f
    srcreg = srcspec <= 0x7 /* src, dst = rmode? */
    dstreg = dstspec <= 0x7
    PC(PC + 2 & 0xffff) /* incr PC, mod 65k */

    // Start of decode ***********************************************************************************
    /* decode IR<15:12> */
    ((IR >> 12) & 0xf) match {

      /* Opcode 0: no operands, specials, branches, JSR, SOPs */
      case 0 =>
        /* decode IR<11:6> */
        ((IR >> 6) & 0x3f) match {
          case 0 =>
            /* no operand */
            if (IR.intValue >= 0x8) {
              /* 000010 - 000077 */
              setTRAP(PDP11.TRAP_ILL.intValue) /* illegal */
              // break
            }
            else IR match {
              case 0 => /* HALT */
                if ((cm == MD_KER) && (!CPUT(CPUOPT.CPUT_J) || ((MAINT & MAINT_HTRAP) == 0)))
                  reason = STOP_HALT
                else if (CPUT(CPUOPT.HAS_HALT4)) {
                  /* priv trap? */
                  setTRAP(PDP11.TRAP_PRV.intValue)
                  setCPUERR(PDP11.CPUE_HALT)
                }
                else setTRAP(PDP11.TRAP_ILL.intValue) /* no, ill inst */
              //break;
              case 1 => /* WAIT */
                wait_state = 1;
              //break;
              case 3 => /* BPT */
                setTRAP(PDP11.TRAP_BPT.intValue)
              //break;
              case 4 => /* IOT */
                setTRAP(PDP11.TRAP_IOT.intValue)
              //break;
              case 5 => /* RESET */
                if (cm == MD_KER) {
                  reset_all(2); /* skip CPU, sys reg */
                  PIRQ ( 0) /* clear PIRQ */
                  STKLIM (0) /* clear STKLIM */
                  MMU.MMR0(0) /* clear MMR0 */
                  MMU.MMR3(0) /* clear MMR3 */
                  cpu_bme = false /* (also clear bme) */
                  for (i <- 0 to PDP11.IPL_HLVL) int_req(i) = 0
                  trap_req = trap_req & ~PDP11.TRAP_INT
                  MMU.dsenable = MMU.calc_ds(cm)
                }
              //break;
              case 6 => /* RTT */
                if (!CPUT(CPUOPT.HAS_RTT)) {
                  setTRAP(PDP11.TRAP_ILL.intValue)
                  //break;
                }
              case 2 => /* RTI */
                src = MMU.ReadW(SP | MMU.dsenable);
                src2 = MMU.ReadW(((SP + 2) & 0xffff) | MMU.dsenable);
                STACKFILE(cm) = {
                  SP((SP + 4) & 0xffff)
                  SP
                };
                oldrs = rs;
                put_PSW(src2, (cm != MD_KER)); /* store PSW, prot */
                if (rs != oldrs) {
                  for (i <- 0 to 6) {
                    REGFILE(i)(oldrs) = R(i)
                    R(i) = REGFILE(i)(rs)
                  }
                }
                SP(STACKFILE(cm))
                MMU.isenable = MMU.calc_is(cm)
                MMU.dsenable = MMU.calc_ds(cm);
                trap_req = MMU.calc_ints(ipl, trap_req);
                JMP_PC(src);
                if (CPUT(CPUOPT.HAS_RTT) && tbit && /* RTT impl? */
                  (IR == 0x2))
                  setTRAP(PDP11.TRAP_TRC.intValue) /* RTI immed trap */
              //break;
              case 7 => /* MFPT */
                if (CPUT(CPUOPT.HAS_MFPT)) /* implemented? */
                  R(0) = cpu_tab(cpu_model).mfpt; /* get type */
                else setTRAP(PDP11.TRAP_ILL.intValue)
              //break;
            } /* end switch no ops */
          //break;                                      /* end case no ops */


          case 1 => /* JMP */
            if (dstreg)
              setTRAP(CPUT({if (CPUOPT.HAS_JREG4 == true) PDP11.TRAP_PRV.intValue else PDP11.TRAP_ILL.intValue}))
            else {
              dst = MMU.GeteaW(dstspec) & 0xffff /* get eff addr */
              if (CPUT(CPUOPT.CPUT_05 | CPUOPT.CPUT_20) && /* 11/05, 11/20 */
                ((dstspec & 0x38) == 0x10)) /* JMP (R)+? */
                dst = R(dstspec & 0x7) /* use post incr */
              JMP_PC(dst);
            }
          //break;                                      /* end JMP */

          case 2 => /* RTS et al*/
            if (IR.intValue < 0x88) {
              /* RTS */
              dstspec = dstspec & 0x7
              JMP_PC(R(dstspec));
              R(dstspec)(MMU.ReadW(SP | MMU.dsenable))
              if (dstspec != 6) SP((SP + 2) & 0xffff)
              //break;
            } else {
              /* end if RTS */
              if (IR.intValue < 0x98) {
                setTRAP(PDP11.TRAP_ILL.intValue);
                //break;
              } else {
                if (IR.intValue < 0xa0) {
                  /* SPL */
                  if (CPUT(CPUOPT.HAS_SPL)) {
                    if (cm == MD_KER)
                      ipl = IR & 0x7
                    trap_req = MMU.calc_ints(ipl, trap_req);
                  }
                  else setTRAP(PDP11.TRAP_ILL.intValue)
                  // break;
                } else {
                  /* end if SPL */
                  if (IR.intValue < 0xb0) {
                    /* clear CC */
                    if ((IR & 0x8) != 0)N = false
                    if ((IR & 0x4) != 0)Z = false
                    if ((IR & 2) != 0) V = false
                    if ((IR & 1) != 0)C = false
                    // break;
                  } else {
                    /* end if clear CCs */
                    /* set CC */
                    if ((IR & 0x8) != 0) N = 1
                    if ((IR & 4) != 0)Z = true
                    if ((IR & 2) != 0) V = 1
                    if ((IR & 1) != 0)C = true
                    // break; /* end case RTS et al */
                  }
                }
              }
            }

          case 3 => /* SWAB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = ((dst & 0xff) << 8) | ((dst >> 8) & 0xff);
            N = GET_SIGN_B(dst & 0xff)
            Z = GET_Z(dst & 0xff)
            if (!CPUT(CPUOPT.CPUT_20))
              V = false;
           C = false;
            if (dstreg) R(dstspec)(dst);
            else MMU.PWriteW(dst, MMU.last_pa);
          //break;                                      /* end SWAB */

          case 4 | 5 => /* BR */
            BRANCH_F(IR)
          //break;

          case 0x6 | 0x7 => /* BR */
            BRANCH_B(IR)
          //break;

          case 0x8 | 0x9 => /* BNE */
            if (Z == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0xa | 0xb => /* BNE */
            if (Z == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0xc | 0xd => /* BEQ */
            if (Z) {
              BRANCH_F(IR)
            }
          //break;

          case 0xe | 0xf => /* BEQ */
            if (Z) {
              BRANCH_B(IR)
            }
          //break;

          case 0x10 | 0x11 => /* BGE */
            if ((N ^ V) == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x12 | 0x13 => /* BGE */
            if ((N ^ V) == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x14 | 0x15 => /* BLT */
            if (N ^ V) {
              BRANCH_F(IR);
            }
          //break;

          case 0x16 | 0x17 => /* BLT */
            if (N ^ V) {
              BRANCH_B(IR)
            }
          //break;

          case 0x18 | 0x19 => /* BGT */
            if ((Z | (N ^ V)) == 0) {
              BRANCH_F(IR);
            }
          //break;

          case 0x1a | 0x1b => /* BGT */
            if ((Z | (N ^ V)) == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x1c | 0x1d => /* BLE */
            if (Z | (N ^ V)) {
              BRANCH_F(IR);
            }
          //break;

          case 0x1e | 0x1f => /* BLE */
            if (Z | (N ^ V)) {
              BRANCH_B(IR)
            }
          //break;

          case 0x20 | 0x21 | 0x22 | 0x23 | /* JSR */
               0x24 | 0x25 | 0x26 | 0x27 =>
            if (dstreg) {
              setTRAP(if (CPUT(CPUOPT.HAS_JREG4) == true) PDP11.TRAP_PRV.intValue else PDP11.TRAP_ILL.intValue)
            } else {
              srcspec = srcspec & 0x7
              dst = MMU.GeteaW(dstspec)
              if (CPUT(CPUOPT.CPUT_05 | CPUOPT.CPUT_20) && /* 11/05, 11/20 */
                ((dstspec & 0x38) == 0x10)) /* JSR (R)+? */
                dst = R(dstspec & 0x7) /* use post incr */
              SP((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6);
              if (MMU.update_MM) MMU.MMR1 ( MMU.reg_mods)
              MMU.WriteW(R(srcspec), SP | MMU.dsenable)
              if ((cm == MD_KER) && (SP < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP)
              R(srcspec) = PC;
              JMP_PC(dst & 0xffff)
            }
          //break;                                      /* end JSR */

          case 0x28 => /* CLR */
           N = false
            V = false
           C = false
            Z = true
            if (dstreg) R(dstspec) ( 0)
            else MMU.WriteW(0, MMU.GeteaW(dstspec))
            //break;

          case 0x29 => /* COM */
            dst = if(dstreg)  R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = dst ^ 0xffff
            N = GET_SIGN_W(dst);
            Z = GET_Z(dst)
            V = false
            C = true
            if (dstreg) R(dstspec) else MMU.PWriteW(dst, MMU.last_pa)
            //break;

          case 0x2a => /* INC */
            dst = if (dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (dst + 1) & 0xffff
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            V = if(dst == 0x8000) -1 else 0
            if (dstreg) R(dstspec) = dst
            else MMU.PWriteW(dst, MMU.last_pa)
          //break;

          case 0x2b => /* DEC */
            dst = if (dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (dst - 1) & 0xffff
            N = GET_SIGN_W(dst);
            Z = GET_Z(dst);
            V = (if(dst == 0x7fff) -1 else 0)
            if (dstreg) R(dstspec) ( dst )  else R(dstspec) (MMU.PWriteW(dst, MMU.last_pa))
          //break;

          case 0x2c => /* NEG */
            dst = if(dstreg)  R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (-dst) & 0xffff
            N = GET_SIGN_W(dst);
            Z = GET_Z(dst);
            V = (if(dst == 0x8000) -1 else 0)
            C = Z ^ 1
            if (dstreg)
              R(dstspec) (dst)
            else MMU.PWriteW(dst, MMU.last_pa)
//            break;

          case 0x2d => /* ADC */
            dst = if(dstreg)  R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (dst + C) & 0xffff
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            V = (C && (if(dst == 0x8000) -1 else 0))
            C = C & Z
            if (dstreg) R(dstspec) ( dst)
            else MMU.PWriteW(dst, MMU.last_pa)
//            break;

          case 0x2e => /* SBC */
            dst = if(dstreg)  R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (dst - C) & 0xffff
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            V = (C && (dst == 0x7fff))
            C = (C && (dst == 0xffff))
            if (dstreg)
              R(dstspec) ( dst)
            else MMU.PWriteW(dst, MMU.last_pa)
            //break;

          case 0x2f => /* TST */
            dst = if(dstreg) R(dstspec)else MMU.ReadW(MMU.GeteaW(dstspec))
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            V = false
           C = false
            //break;

          case 0x30 => /* ROR */
            src = if(dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (src >> 1) | (C << 15)
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            C = (src & 1)
            V = N ^ C
            if (dstreg) R(dstspec) (dst) else MMU.PWriteW(dst, MMU.last_pa)
            //break;

          case 0x31 => /* ROL */
            src = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec));
            dst = ((src << 1) | C) & 0xffff
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            C = GET_SIGN_W(src)
            V = N ^ C
            if (dstreg) R(dstspec) (dst)else MMU.PWriteW(dst, MMU.last_pa)
            //break

          case 0x32 => /* ASR */
            src = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec));
            dst = (src >> 1) | (src & 0x8000)
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            C = (src & 1)
            V = N ^ C
            if (dstreg) R(dstspec) (dst) else MMU.PWriteW(dst, MMU.last_pa)
            //break;

          case 0x33 => /* ASL */
            src = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
            dst = (src << 1) & 0xffff
            N = GET_SIGN_W(dst)
            Z = GET_Z(dst)
            C = GET_SIGN_W(src)
            V = N ^ C;
            if (dstreg) R(dstspec) (dst) else MMU.PWriteW(dst, MMU.last_pa)
            //break;

          /* Notes:
         - MxPI must mask GeteaW returned address to force ispace
         - MxPI must set MMR1 for SP recovery in case of fault
      */

          case 0x34 => /* MARK */
            if (CPUT(CPUOPT.HAS_MARK)) {
              i = (PC + dstspec + dstspec) & 0xffff
              JMP_PC(R(5))
              R(5)( MMU.ReadW(i | MMU.dsenable))
              SP((i + 2) & 0xffff)
            }
            else setTRAP(PDP11.TRAP_ILL);
          //break;

          case 0x35 => /* MFPI */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm)) dst = STACKFILE(pm)
                else dst = R(dstspec)
              }
              else {
                i = if ((cm == pm) && (cm == MD_USR)) MMU.calc_ds(pm) else MMU.calc_is(pm)
                dst = MMU.ReadW((MMU.GeteaW(dstspec) & 0xffff) | i)
              }
              N = GET_SIGN_W(dst)
              Z = GET_Z(dst)
              V = false
              SP ((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6)
              if (MMU.update_MM) MMU.MMR1( MMU.reg_mods)
              MMU.WriteW(dst, SP | MMU.dsenable)
              if ((cm == MD_KER) && (SP < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP);
            }
            else setTRAP(PDP11.TRAP_ILL);
          //break;

          case 0x36 => /* MTPI */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              dst = MMU.ReadW(SP | MMU.dsenable);
              N = GET_SIGN_W(dst);
              Z = GET_Z(dst);
              V = false
              SP ((SP + 2) & 0xffff)
              MMU.reg_mods = 0x16
              if (MMU.update_MM) MMU.MMR1 (MMU.reg_mods)
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  STACKFILE(pm)(dst)
                else R(dstspec) ( dst);
              }
              else MMU.WriteW(dst, (MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_is(pm));
            }
            else setTRAP(PDP11.TRAP_ILL);
          //break;

          case 0x37 => /* SXT */
            if (CPUT(CPUOPT.HAS_SXS)) {
              dst = if(N != 0) 0xffff else 0
              Z = N ^ 1
              V = false
              if (dstreg)
                R(dstspec) (dst)
              else MMU.WriteW(dst, MMU.GeteaW(dstspec));
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
            //break;

          case 0x38 => /* CSM */
            if (CPUT(CPUOPT.HAS_CSM) && (MMU.MMR3 & MMU.MMR3_CSM) && (cm != MD_KER)) {
              dst = if(dstreg) R(dstspec) else MMU.ReadW(MMU.GeteaW(dstspec))
              PSW = get_PSW() & ~PSW_CC /* PSW, cc = 0 */
              STACKFILE(cm)( SP)
              MMU.WriteW(PSW, ((SP - 2) & 0xffff) | MMU.calc_ds(MD_SUP));
              MMU.WriteW(PC, ((SP - 4) & 0xffff) | MMU.calc_ds(MD_SUP));
              MMU.WriteW(dst, ((SP - 6) & 0xffff) | MMU.calc_ds(MD_SUP));
              SP ((SP - 6) & 0xffff)
              pm = cm
              cm = MD_SUP
              tbit = 0
              MMU.isenable = MMU.calc_is(cm)
              MMU.dsenable = MMU.calc_ds(cm)
              PC (MMU.ReadW(0x8 | MMU.isenable))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
            //break;

          case 0x3a => /* TSTSET */
            if (CPUT(CPUOPT.HAS_TSWLK) && !dstreg) {
              dst = MMU.ReadMW(MMU.GeteaW(dstspec));
              N = GET_SIGN_W(dst);
              Z = GET_Z(dst);
              V = false;
              C = (dst & 1);
              R(0)( dst) /* R[0] <- dst */
              MMU.PWriteW(R(0) | 1, MMU.last_pa) /* dst <- R[0] | 1 */
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
            //break;

          case 0x3b => /* WRTLCK */
            if (CPUT(CPUOPT.HAS_TSWLK) && !dstreg) {
              N = GET_SIGN_W(R(0))
              Z = GET_Z(R(0))
              V = false
              MMU.WriteW(R(0), MMU.GeteaW(dstspec))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
          //break;

          case _ =>
            setTRAP(PDP11.TRAP_ILL.intValue)
          //break;
        } /* end switch SOPs */
      //break;                                          /* end case 000 */

      /* Opcodes 01 - 06: double operand word instructions
       J-11 (and F-11) optimize away register source operand decoding.
       As a result, dop R,+/-(R) use the modified version of R as source.
       Most (but not all) other PDP-11's fetch the source operand before
       any destination operand decoding.
       Add: v = [sign (src) = sign (src2)] and [sign (src) != sign (result)]
       Cmp: v = [sign (src) != sign (src2)] and [sign (src2) = sign (result)]
    */

      case 1 => /* MOV */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          ea = MMU.GeteaW(dstspec);
          dst = R(srcspec)
        } else {
          dst = if (srcreg) R(srcspec) else MMU.ReadW(MMU.GeteaW(srcspec));
          if (!dstreg)
            ea = MMU.GeteaW(dstspec)
        }
        N = GET_SIGN_W(dst);
        Z = GET_Z(dst);
        V = false;
        if (dstreg) R(dstspec) (dst)
        else MMU.WriteW(dst, ea);
      //break;

      case 2 => /* CMP */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadW(MMU.GeteaW(dstspec));
          src = R(srcspec);
        }
        else {
          src = if(srcreg) R(srcspec) else  MMU.ReadW(MMU.GeteaW(srcspec));
          src2 = if(dstreg) R(dstspec)else  MMU.ReadW(MMU.GeteaW(dstspec));
        }
        dst = (src - src2) & 0xffff
        N = GET_SIGN_W(dst);
        Z = GET_Z(dst);
        V = GET_SIGN_W((src ^ src2) & (~src2 ^ dst));
        C = (src < src2);
        //break;

      case 3 => /* BIT */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadW(MMU.GeteaW(dstspec));
          src = R(srcspec);
        }
        else {
          src = if(srcreg) R(srcspec) else  MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if(dstreg) R(dstspec) else  MMU.ReadW(MMU.GeteaW(dstspec))
        }
        dst = src2 & src
        N = GET_SIGN_W(dst);
        Z = GET_Z(dst);
        V = false;
        //break;

      case 4 => /* BIC */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec));
          src = R(srcspec);
        }
        else {
          src = if(srcreg)  R(srcspec) else MMU.ReadW(MMU.GeteaW(srcspec));
          src2 = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec));
        }
        dst = src2 & ~src
        N = GET_SIGN_W(dst);
        Z = GET_Z(dst);
        V = false
        if (dstreg)
          R(dstspec) ( dst)
        else MMU.PWriteW(dst, MMU.last_pa)
        //break;

      case 5 => /* BIS */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec)
        }
        else {
          src = if(srcreg) R(srcspec) else  MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = src2 | src
        N = GET_SIGN_W(dst)
        Z = GET_Z(dst)
        V = false
        if (dstreg) R(dstspec) (dst)
        else MMU.PWriteW(dst, MMU.last_pa)
      //break;

      case 6 => /* ADD */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec)
        }
        else {
          src = if (srcreg) R(srcspec) else MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if (dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = (src2 + src) & 0xffff
        N = GET_SIGN_W(dst)
        Z = GET_Z(dst)
        V = GET_SIGN_W((~src ^ src2) & (src ^ dst))
        C = (dst < src)
        if (dstreg)
          R(dstspec) (dst)
        else MMU.PWriteW(dst, MMU.last_pa)
      //break;

      /* Opcode 07: EIS, FIS, CIS
       Notes:
       - The code assumes that the host int length is at least 32 bits.
       - MUL carry: C is set if the (signed) result doesn't fit in 16 bits.
       - Divide has three error cases:
            1. Divide by zero.
            2. Divide largest negative number by -1.
            3. (Signed) quotient doesn't fit in 16 bits.
         Cases 1 and 2 must be tested in advance, to avoid C runtime errors.
       - ASHx left: overflow if the bits shifted out do not equal the sign
         of the result (convert shift out to 1/0, xor against sign).
       - ASHx right: if right shift sign extends, then the shift and
         conditional or of shifted -1 is redundant.  If right shift zero
         extends, then the shift and conditional or does sign extension.
    */

      case 0x7 =>
        srcspec = srcspec & 0x7;
        ((IR >> 9) & 0x7) match {
          /* decode IR<11:9> */

          case 0 => /* MUL */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL)

            } else {
              src2 = if (dstreg) R(dstspec) else MMU.ReadW(MMU.GeteaW(dstspec))
              src = R(srcspec)
              if (GET_SIGN_W(src2)) src2 = src2 | ~0x7fff
              if (GET_SIGN_W(src)) src = src | ~0x7fff
              dst = src * src2
              R(srcspec) ( (dst >> 16) & 0xffff)
              R(srcspec | 1) ( dst & 0xffff)
              N = (dst < 0)
              Z = GET_Z(dst)
              V = false
              C = ((dst > 0x7fff) || (dst < -0x8000))
            }
          //break;

          case 1 => /* DIV */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue)

            } else {
              src2 = if (dstreg) R(dstspec) else MMU.ReadW(MMU.GeteaW(dstspec))
              src = (R(srcspec) << 16) | R(srcspec | 1)
              if (src2 == 0) {
               N = false /* J11,11/70 compat */
                Z =1
                V =1
               C = true
                //break;
              } else {
                if ((src == 0x80000000) && (src2 == 0xffff)) {
                  V = 1 /* J11,11/70 compat */
                 N = false
                 Z = false
                 C = false /* N =Z = false */
                  //break;
                } else {
                  if (GET_SIGN_W(src2))
                    src2 = src2 | ~0x7fff;
                  if (GET_SIGN_W(R(srcspec)))
                    src = src | ~0x7fffffff;
                  dst = src / src2
                  N = (if(dst < 0) 1 else 0) /* N set on 32b result */
                  if ((dst > 0x7fff) || (dst < -0x8000)) {
                    V = 1 /* J11,11/70 compat */
                   Z = false
                   C = false /* Z =C = false */
                    //break;
                  } else {
                    R(srcspec) ( dst & 0xffff)
                    R(srcspec | 1) ((src - (src2 * dst)) & 0xffff)
                    Z = GET_Z(dst)
                    V = false
                   C = false
                    //break;
                  }
                }
              }
            }

          case 2 => /* ASH */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue);
              //break;
            } else {
              src2 = if (dstreg) R(dstspec) else MMU.ReadW(MMU.GeteaW(dstspec));
              src2 = src2 & 0x3f;
              sign = GET_SIGN_W(R(srcspec));
              src = if (sign != 0) R(srcspec) | ~0x7fff else R(srcspec)
              if (src2 == 0) {
                /* [0] */
                dst = src
                V = false
               C = false
              }
              else if (src2 <= 15) {
                /* [1,15] */
                dst = src << src2
                i = (src >> (16 - src2)) & 0xffff
                V = if(i != (dst & 0x8000)  ) 0xffff else 0
                C = (i & 1)
              }
              else if (src2 <= 31) {
                /* [16,31] */
                dst = 0;
                V = if(src != 0) 1 else 0
                C = (src << (src2 - 16)) & 1
              }
              else if (src2 == 32) {
                /* [32] = -32 */
                dst = -sign
                V = false;
                C = sign
              }
              else {
                /* [33,63] = -31,-1 */
                dst = (src >> (64 - src2)) | (-sign << (src2 - 32))
                V = false
                C = ((src >> (63 - src2)) & 1)
              }
              dst = {
                R(srcspec) ( dst & 0xffff)
                R(srcspec)
              }
              N = GET_SIGN_W(dst);
              Z = GET_Z(dst);
              //break;
            }
          case 3 => /* ASHC */
            if (!CPUO(CPUOPT.OPT_EIS)) {
              setTRAP(PDP11.TRAP_ILL.intValue);
              //break;
            } else {
              src2 = if (dstreg) R(dstspec) else MMU.ReadW(MMU.GeteaW(dstspec))
              src2 = src2 & 0x3f
              sign = GET_SIGN_W(R(srcspec));
              src = (R(srcspec) << 16) | R(srcspec | 1)
              if (src2 == 0) {
                /* [0] */
                dst = src;
                V = false
               C = false
              }
              else if (src2 <= 31) {
                /* [1,31] */
                dst = (src) << src2
                i = (src >> (32 - src2)) | (-sign << src2);
                V = (if(i != (dst & 0x80000000))  -1 else  0)
                C = (i & 1)
              }
              else if (src2 == 32) {
                /* [32] = -32 */
                dst = -sign
                V = false
                C = sign
              }
              else {
                /* [33,63] = -31,-1 */
                dst = (src >> (64 - src2)) | (-sign << (src2 - 32));
                V = false;
                C = ((src >> (63 - src2)) & 1)
              }
              i = {
                R(srcspec) ((dst >> 16) & 0xffff)
              R(srcspec)
              }
              dst = {
                R(srcspec | 1) ( dst & 0xffff)
                R(srcspec | 1)
              }
              N = GET_SIGN_W(i)
              Z = GET_Z(dst | i)
              //break;
            }
          case 4 => /* XOR */
            if (CPUT(CPUOPT.HAS_SXS)) {
              if (CPUT(CPUOPT.IS_SDSD) && !dstreg) {
                /* R,not R */
                src2 = MMU.ReadMW(MMU.GeteaW(dstspec));
                src = R(srcspec)
              }
              else {
                src = R(srcspec)
                src2 = if(dstreg) R(dstspec) else MMU.ReadMW(MMU.GeteaW(dstspec))
              }
              dst = src ^ src2
              N = GET_SIGN_W(dst)
              Z = GET_Z(dst)
              V = false
              if (dstreg) R(dstspec) ( dst)
              else MMU.PWriteW(dst, MMU.last_pa)
            }
            else setTRAP(PDP11.TRAP_ILL)
          //break;

          case 5 => /* FIS */
            if (CPUO(CPUOPT.OPT_FIS))
              fis11(IR)
            else setTRAP(PDP11.TRAP_ILL)
          //break;

          case 6 => /* CIS */
            if (CPUT(CPUOPT.CPUT_60) && (cm == MD_KER) && /* 11/60 MED? */
              (IR == 0x7d80)) {
              MMU.ReadE(PC | MMU.isenable) /* read immediate */
              PC ( (PC + 2) & 0xffff)
            }
            else if (CPUO(CPUOPT.OPT_CIS)) /* CIS option? */
              reason = cis11(IR)
            else setTRAP(PDP11.TRAP_ILL.intValue)
          //break;

          case 7 => /* SOB */
            if (CPUT(CPUOPT.HAS_SXS)) {
              R(srcspec) ( (R(srcspec) - 1) & 0xffff)
              if (R(srcspec) != 0) {
                JMP_PC((PC - dstspec - dstspec) & 0xffff)
              }
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
          //break;
        } /* end switch EIS */
      //break;                                          /* end case 007 */

      /* Opcode 10: branches, traps, SOPs */

      case 0x8 =>
        ((IR >> 6) & 0x3f) match {
          /* decode IR<11:6> */

          case 0x0 | 0x1 => /* BPL */
            if (N == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x2 | 0x3 => /* BPL */
            if (N == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x4 | 0x5 => /* BMI */
            if (N!= 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x6 | 0x7 => /* BMI */
            if (N!= 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x08 | 0x09 => /* BHI */
            if ((C | Z) == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0xa | 0xb => /* BHI */
            if ((C | Z) == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0xc | 0xd => /* BLOS */
            if (C != 0 | Z != 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0xe | 0xf => /* BLOS */
            if (C != 0| Z != 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x10 | 0x11 => /* BVC */
            if (V == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x12 | 0x13 => /* BVC */
            if (V == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x14 | 0x15 => /* BVS */
            if (V!= 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x16 | 0x17 => /* BVS */
            if (V!= 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x18 | 0x19 => /* BCC */
            if (C == 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x1a | 0x1b => /* BCC */
            if (C == 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x1c | 0x1d => /* BCS */
            if (C!= 0) {
              BRANCH_F(IR)
            }
          //break;

          case 0x1e | 0x1f => /* BCS */
            if (C!= 0) {
              BRANCH_B(IR)
            }
          //break;

          case 0x20 | 0x21 | 0x22 | 0x23 => /* EMT */
            setTRAP(PDP11.TRAP_EMT.intValue)
          //break;

          case 0x24 | 0x25 | 0x26 | 0x27 => /* TRAP */
            setTRAP(PDP11.TRAP_TRAP.intValue)
          //break;

          case 0x28 => /* CLRB */
           N = false
            V = false
           C = false
           Z = true
            if (dstreg) R(dstspec) ( R(dstspec) & 0xff00)
            else MMU.WriteB(0, MMU.GeteaB(dstspec))
          //break;

          case 0x29 => /* COMB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (dst ^ 0xff) & 0xff
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            V = false
           C = true
            if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2a => /* INCB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (dst + 1) & 0xff
            N = GET_SIGN_B(dst);
            Z = GET_Z(dst);
            V = (dst == 0x80);
            if (dstreg)
              R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2b => /* DECB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (dst - 1) & 0xff;
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            V = (dst == 0x7f)
            if (dstreg)
              R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2c => /* NEGB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (-dst) & 0xff;
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst);
            V = (dst == 0x80)
            C = (Z ^ 1)
            if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2d => /* ADCB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (dst + C) & 0xff
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            V = (C && (if(dst == 0x80) -1 else 0))
            C = C & Z
            if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2e => /* SBCB */
            dst = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (dst - C) & 0xff
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst);
            V = (C && (dst == 0x7f))
            C = (C && (dst == 0xff))
            if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x2f => /* TSTB */
            dst = if (dstreg) R(dstspec) & 0xff else MMU.ReadB(MMU.GeteaB(dstspec))
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            V = false
           C = false
          //break;

          case 0x30 => /* RORB */
            src = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = ((src & 0xff) >> 1) | (C << 7)
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            C = (src & 1)
            V = N ^ C
            if (dstreg) R(dstspec) ((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x31 => /* ROLB */
            src = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = ((src << 1) | C) & 0xff
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            C = GET_SIGN_B(src & 0xff)
            V = N ^ C
            if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x32 => /* ASRB */
            src = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec));
            dst = ((src & 0xff) >> 1) | (src & 0x80);
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst)
            C = (src & 1)
            V = N ^ C
            if (dstreg)
              R(dstspec) = (R(dstspec) & 0xff00) | dst
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          case 0x33 => /* ASLB */
            src = if (dstreg) R(dstspec) else MMU.ReadMB(MMU.GeteaB(dstspec))
            dst = (src << 1) & 0xff
            N = GET_SIGN_B(dst)
            Z = GET_Z(dst);
            C = GET_SIGN_B(src & 0xff)
            V = N ^ C;
            if (dstreg) R(dstspec) ((R(dstspec) & 0xff00) | dst)
            else MMU.PWriteB(dst, MMU.last_pa)
          //break;

          /* Notes:
         - MTPS cannot alter the T bit
         - MxPD must mask GeteaW returned address, dspace is from cm not pm
         - MxPD must set MMR1 for SP recovery in case of fault
      */

          case 0x34 => /* MTPS */
            if (CPUT(CPUOPT.HAS_MXPS)) {
              dst = if (dstreg) R(dstspec) else MMU.ReadB(MMU.GeteaB(dstspec))
              if (cm == MD_KER) {
                ipl = (dst >> PDP11.PSW_V_IPL) & 0x7
                trap_req = MMU.calc_ints(ipl, trap_req)
              }
              N = (dst >> PDP11.PSW_V_N) & 1
              Z = (dst >> PDP11.PSW_V_Z) & 1
              V = (dst >> PDP11.PSW_V_V) & 1
              C = (dst >> PDP11.PSW_V_C) & 1
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
          //break;

          case 0x35 => /* MFPD */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  dst = STACKFILE(pm)
                else dst = R(dstspec)
              }
              else dst = MMU.ReadW((MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_ds(pm))
              N = GET_SIGN_W(dst)
              Z = GET_Z(dst)
              V = false
              SP((SP - 2) & 0xffff)
              MMU.reg_mods = MMU.calc_MMR1(0xf6)
              if (MMU.update_MM) MMU.MMR1 = MMU.reg_mods
              MMU.WriteW(dst, SP | MMU.dsenable);
              if ((cm == MD_KER) && (SP < (STKLIM + PDP11.STKL_Y)))
                set_stack_trap(SP)
            }
            else setTRAP(PDP11.TRAP_ILL)
          //break;

          case 0x36 => /* MTPD */
            if (CPUT(CPUOPT.HAS_MXPY)) {
              dst = MMU.ReadW(SP | MMU.dsenable)
              N = GET_SIGN_W(dst)
              Z = GET_Z(dst)
              V = false
              SP((SP + 2) & 0xffff)
              MMU.reg_mods = 0x16
              if (MMU.update_MM)
                MMU.MMR1 = MMU.reg_mods
              if (dstreg) {
                if ((dstspec == 6) && (cm != pm))
                  STACKFILE(pm) ( dst)
                else R(dstspec) ( dst)
              }
              else MMU.WriteW(dst, (MMU.GeteaW(dstspec) & 0xffff) | MMU.calc_ds(pm))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
          //break;

          case 0x37 => /* MFPS */
            if (CPUT(CPUOPT.HAS_MXPS)) {
              dst = get_PSW() & 0xff
              N = GET_SIGN_B(dst)
              Z = GET_Z(dst)
              V = false;
              if (dstreg) R(dstspec) (if (dst & 0x80) 0xff00 | dst else dst)
              else MMU.WriteB(dst, MMU.GeteaB(dstspec))
            }
            else setTRAP(PDP11.TRAP_ILL.intValue)
          // break;

          case _ =>
            setTRAP(PDP11.TRAP_ILL.intValue)
          //break;
        } /* end switch SOPs */
      //break;                                          /* end case 010 */

      /* Opcodes 11 - 16: double operand byte instructions
       Cmp: v = [sign (src) != sign (src2)] and [sign (src2) = sign (result)]
       Sub: v = [sign (src) != sign (src2)] and [sign (src) = sign (result)]
    */

      case 0x9 => /* MOVB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          ea = MMU.GeteaB(dstspec);
          dst = R(srcspec) & 0xff;
        }
        else {
          dst = if (srcreg) R(srcspec) & 0xff else MMU.ReadB(MMU.GeteaB(srcspec));
          if (!dstreg)
            ea = MMU.GeteaB(dstspec)
        }
        N = GET_SIGN_B(dst)
        Z = GET_Z(dst)
        V = false
        if (dstreg)
          R(dstspec) (if((dst & 0x80) != 0)  0xff00 | dst else  dst)
        else MMU.WriteB(dst, ea)
      //break;

      case 0xa => /* CMPB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadB(MMU.GeteaB(dstspec))
          src = R(srcspec) & 0xff
        }
        else {
          src = if(srcreg) R(srcspec) & 0xff else  MMU.ReadB(MMU.GeteaB(srcspec))
          src2 = if(dstreg)  R(dstspec) & 0xff else  MMU.ReadB(MMU.GeteaB(dstspec))
        }
        dst = (src - src2) & 0xff
        N = GET_SIGN_B(dst)
        Z = GET_Z(dst)
        V = GET_SIGN_B((src ^ src2) & (~src2 ^ dst))
        C = (src < src2)
        //break;

      case 0xb => /* BITB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadB(MMU.GeteaB(dstspec))
          src = R(srcspec) & 0xff
        }
        else {
          src = if(srcreg)  R(srcspec) & 0xff else  MMU.ReadB(MMU.GeteaB(srcspec))
          src2 = if(dstreg) R(dstspec) & 0xff else  MMU.ReadB(MMU.GeteaB(dstspec))
        }
        dst = (src2 & src) & 0xff
        N = GET_SIGN_B(dst)
        Z = GET_Z(dst)
        V = false
        //break;

      case 0xc => /* BICB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMB(MMU.GeteaB(dstspec))
          src = R(srcspec)
        }
        else {
          src = if(srcreg)  R(srcspec) else  MMU.ReadB(MMU.GeteaB(srcspec))
          src2 = if(dstreg) R(dstspec) else  MMU.ReadMB(MMU.GeteaB(dstspec))
        }
        dst = (src2 & ~src) & 0xff
        N = GET_SIGN_B(dst)
        Z = GET_Z(dst)
        V = false
        if (dstreg)
          R(dstspec) ( (R(dstspec) & 0xff00) | dst)
        else MMU.PWriteB(dst, MMU.last_pa)
        //break;

      case 0xd => /* BISB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMB(MMU.GeteaB(dstspec))
          src = R(srcspec);
        }
        else {
          src = if(srcreg) R(srcspec) else  MMU.ReadB(MMU.GeteaB(srcspec))
          src2 = if(dstreg) R(dstspec) else  MMU.ReadMB(MMU.GeteaB(dstspec))
        }
        dst = (src2 | src) & 0xff
        N = GET_SIGN_B(dst)
        Z = GET_Z(dst);
        V = false
        if (dstreg) R(dstspec) ( (R(dstspec) & 0xff00) | dst)
        else MMU.PWriteB(dst, MMU.last_pa)
        //break;

      case 0xe => /* SUB */
        if (CPUT(CPUOPT.IS_SDSD) && srcreg && !dstreg) {
          /* R,not R */
          src2 = MMU.ReadMW(MMU.GeteaW(dstspec))
          src = R(srcspec)
        }
        else {
          src = if(srcreg)  R(srcspec) else  MMU.ReadW(MMU.GeteaW(srcspec))
          src2 = if(dstreg) R(dstspec) else  MMU.ReadMW(MMU.GeteaW(dstspec))
        }
        dst = (src2 - src) & 0xffff
        N = GET_SIGN_W(dst)
        Z = GET_Z(dst)
        V = GET_SIGN_W((src ^ src2) & (~src ^ dst))
        C = (src2 < src)
        if (dstreg)
          R(dstspec) ( dst)
        else MMU.PWriteW(dst, MMU.last_pa)
        //break;

      /* Opcode 17: floating point */

      case 0xf =>
        if (CPUO(CPUOPT.OPT_FPP))
          fp11(IR); /* call fpp */
        else setTRAP(PDP11.TRAP_ILL.intValue);
        //break; /* end case 017 */
    } /* end switch op */
  } /* end main loop */


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
  val PCQ_MASK: Int = {
    PCQ_SIZE - 1
  }

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

  def GET_SIGN_W(v: UShort): Boolean = {
    if(((v >> 15) & 1) != 0) true else false
  }
  
  def GET_SIGN_W(v:Int): Boolean = GET_SIGN_W(UShort(v.toShort))

  def GET_SIGN_B(v: UByte): Int = {
    (v >> 7) & 1
  }
  def GET_SIGN_B(v:Int):Int = GET_SIGN_B(UByte(v.toByte))


  def GET_Z(v: Int): Boolean = {
    v == 0
  }

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

  var wait_state: Int = 0
  var trap_req: Int = 0

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
  /*   condition codes */
  var N: Boolean = false
  var Z: Boolean = false
  var V: Boolean = false
  var C: Boolean = false


  override def onHalt(singleStepped: Boolean): Unit = ???

  //override val registers: Map[String, Register] = _
  override def resetCPU(): Unit = {

    PIRQ.set16(0)
    STKLIM.set16(0)
    if (CPUT(CPUOPT.CPUT_T)) /* T11? */
      PSW = 0xe0 /* start at IPL 7 */
    else
      PSW = 0 /* else at IPL 0 */
    MMU.MMR0.set16(0)
    MMU.MMR1.set16(0)
    MMU.MMR2.set16(0)
    MMU.MMR3.set16(0)

    trap_req = 0;
    wait_state = 0;
    //if (M == NULL) {                    /* First time init */
    //  M = (uint16 *) calloc (MEMSIZE >> 1, sizeof (uint16));
    //  if (M == NULL)
    //    return SCPE_MEM;
    //sim_set_pchar (0, "01000023640"); /* ESC, CR, LF, TAB, BS, BEL, ENQ */
    //sim_brk_dflt = SWMASK ('E');
    //sim_brk_types = sim_brk_dflt|SWMASK ('P')|
    //  SWMASK ('R')|SWMASK ('S')|
    //  SWMASK ('W')|SWMASK ('X');
    //sim_brk_type_desc = cpu_breakpoints;
    //sim_vm_is_subroutine_call = &cpu_is_pc_a_subroutine_call;
    //sim_clock_precalibrate_commands = pdp11_clock_precalibrate_commands;
    //auto_config(NULL, 0);           /* do an initial auto configure */

    //pcq_r = find_reg ("PCQ", NULL, dptr);
    //if (pcq_r)
    //  pcq_r->qptr = 0;
    //else
    //  return SCPE_IERR;
    //set_r_display (0, MD_KER);
    //return build_dib_tab ();            /* build, chk dib_tab */
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

  val IOBA_CTL: Int = IOPAGEBASE + 0x1f50
  /* board ctrl */
  val IOLN_CTL = 0x8

  val IOBA_UCA: Int = IOPAGEBASE + 0xff8
  /* UC15 DR11 #1 */
  val IOLN_UCA = 0x6
  val IOBA_UCB: Int = IOPAGEBASE + 0xff0
  /* UC15 DR11 #2 */
  val IOLN_UCB = 0x6
  val IOBA_UBM: Int = IOPAGEBASE + 0x1080
  /* Unibus map */
  //val IOLN_UBM     =   (UBM_LNT_LW * sizeof (int32))
  val IOBA_MMR3: Int = IOPAGEBASE + 0x154e
  /* MMR3 */
  val IOLN_MMR3 = 0x2
  val IOBA_TTI: Int = IOPAGEBASE + 0x1f70
  /* DL11 rcv */
  val IOLN_TTI = 0x4
  val IOBA_TTO: Int = IOPAGEBASE + 0x1f74
  /* DL11 xmt */
  val IOLN_TTO = 0x4
  val IOBA_SR: Int = IOPAGEBASE + 0x1f78
  /* SR */
  val IOLN_SR = 0x2
  val IOBA_MMR012: Int = IOPAGEBASE + 0x1f7a
  /* MMR0-2 */
  val IOLN_MMR012 = 0x6
  val IOBA_GPR: Int = IOPAGEBASE + 0x1fc0
  /* GPR's */
  val IOLN_GPR = 0x8
  val IOBA_UCTL: Int = IOPAGEBASE + 0x1fd8
  /* UBA ctrl */
  val IOLN_UCTL = 0x8
  val IOBA_CPU: Int = IOPAGEBASE + 0x1fe0
  /* CPU reg */
  val IOLN_CPU = 0x1e
  val IOBA_PSW: Int = IOPAGEBASE + 0x1ffe
  /* PSW */
  val IOLN_PSW = 0x2
  val IOBA_UIPDR: Int = IOPAGEBASE + 0x1f80
  /* user APR's */
  val IOLN_UIPDR = 0x10
  val IOBA_UDPDR: Int = IOPAGEBASE + 0x1f90
  val IOLN_UDPDR = 0x10
  val IOBA_UIPAR: Int = IOPAGEBASE + 0x1fa0
  val IOLN_UIPAR = 0x10
  val IOBA_UDPAR: Int = IOPAGEBASE + 0x1fb0
  val IOLN_UDPAR = 0x10
  val IOBA_SUP: Int = IOPAGEBASE + 0x1480
  /* supervisor APR's */
  val IOLN_SUP = 0x40
  val IOBA_KIPDR: Int = IOPAGEBASE + 0x14c0
  /* kernel APR's */
  val IOLN_KIPDR = 0x10
  val IOBA_KDPDR: Int = IOPAGEBASE + 0x14d0
  val IOLN_KDPDR = 0x10
  val IOBA_KIPAR: Int = IOPAGEBASE + 0x14e0
  val IOLN_KIPAR = 0x10
  val IOBA_KDPAR: Int = IOPAGEBASE + 0x14f0
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
