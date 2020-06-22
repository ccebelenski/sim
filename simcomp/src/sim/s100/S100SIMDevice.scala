package sim.s100

import sim.cpu.Z80MMU
import sim.device.PortMappedDevice
import sim.unsigned.{UByte, UInt}
import sim.Utils

/**
  * A pseudo device for communication between the S100 and the simulator
  * Sits on Port 0xfe
  */
class S100SIMDevice(machine: S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDevice(machine, mmu, ports) {
  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {

    action.toInt match {
      case 0xfe => {
        if (isWrite) simh_out(value)
        else simh_in(value)
      }
      case _ => UByte(0)
    }
  }

  override val description: String = "SIM Device"
  override val name = "SIM"

  override def init(): Unit = {}

  override def createUnitOptions: Unit = {}

  override def optionChanged(sb: StringBuilder): Unit = ???

  /*
      printTimeCmd,               /*  0 print the current time in milliseconds                            */
    startTimerCmd,              /*  1 start a new timer on the top of the timer stack                   */
    stopTimerCmd,               /*  2 stop timer on top of timer stack and show time difference         */
    resetPTRCmd,                /*  3 reset the PTR device                                              */
    attachPTRCmd,               /*  4 attach the PTR device                                             */
    detachPTRCmd,               /*  5 detach the PTR device                                             */
    getSIMHVersionCmd,          /*  6 get the current version of the SIMH pseudo device                 */
    getClockZSDOSCmd,           /*  7 get the current time in ZSDOS format                              */
    setClockZSDOSCmd,           /*  8 set the current time in ZSDOS format                              */
    getClockCPM3Cmd,            /*  9 get the current time in CP/M 3 format                             */
    setClockCPM3Cmd,            /* 0x0a set the current time in CP/M 3 format                             */
    getBankSelectCmd,           /* 11 0x0b get the selected bank                                             */
    setBankSelectCmd,           /* 12 0x0c set the selected bank                                             */
    getCommonCmd,               /* 13 0x0d get the base address of the common memory segment                 */
    resetSIMHInterfaceCmd,      /* 14 0x0e reset the SIMH pseudo device                                      */
    showTimerCmd,               /* 15 0x0f show time difference to timer on top of stack                     */
    attachPTPCmd,               /* 16 0x10 attach PTP to the file with name at beginning of CP/M command line*/
    detachPTPCmd,               /* 17 0x11 detach PTP                                                        */
    hasBankedMemoryCmd,         /* 18 0x12 determines whether machine has banked memory                      */
    setZ80CPUCmd,               /* 19 0x13 set the CPU to a Z80                                              */
    set8080CPUCmd,              /* 20 0x14 set the CPU to an 8080                                            */
    startTimerInterruptsCmd,    /* 21 0x15 start timer interrupts                                            */
    stopTimerInterruptsCmd,     /* 22 0x16 stop timer interrupts                                             */
    setTimerDeltaCmd,           /* 23 set the timer interval in which interrupts occur                  */
    setTimerInterruptAdrCmd,    /* 24 set the address to call by timer interrupts                       */
    resetStopWatchCmd,          /* 25 reset the millisecond stop watch                                  */
    readStopWatchCmd,           /* 26 read the millisecond stop watch                                   */
    SIMHSleepCmd,               /* 27 let SIMH sleep for SIMHSleep microseconds                         */
    getHostOSPathSeparatorCmd,  /* 28 obtain the file path separator of the OS under which SIMH runs    */
    getHostFilenamesCmd,        /* 29 perform wildcard expansion and obtain list of file names          */
    readURLCmd,                 /* 30 read the contents of an URL                                       */
    getCPUClockFrequency,       /* 31 get the clock frequency of the CPU                                */
    setCPUClockFrequency, /* 32 set the clock frequency of the CPU                                */
   */

  /*  Z80 or 8080 programs communicate with the SIMH pseudo device via port 0xfe.
        The following principles apply:
    1)  For commands that do not require parameters and do not return results
        ld  a,<cmd>
        out (0feh),a
        Special case is the reset command which needs to be send 128 times to make
        sure that the internal state is properly reset.
    2)  For commands that require parameters and do not return results
        ld  a,<cmd>
        out (0feh),a
        ld  a,<p1>
        out (0feh),a
        ld  a,<p2>
        out (0feh),a
        ...
        Note: The calling program must send all parameter bytes. Otherwise
        the pseudo device is left in an undefined state.
    3)  For commands that do not require parameters and return results
        ld  a,<cmd>
        out (0feh),a
        in  a,(0feh)    ; <A> contains first byte of result
        in  a,(0feh)    ; <A> contains second byte of result
        ...
        Note: The calling program must request all bytes of the result. Otherwise
        the pseudo device is left in an undefined state.
    4)  For commands that do require parameters and return results
        ld  a,<cmd>
        out (0feh),a
        ld  a,<p1>
        out (0feh),a
        ld  a,<p2>
        out (0feh),a
        ...             ; send all parameters
        in  a,(0feh)    ; <A> contains first byte of result
        in  a,(0feh)    ; <A> contains second byte of result
        ...
*/

  private def simh_out(byte: UByte): UByte = {
    //Utils.outln(s"Write 0xfe - $byte")
    byte.toInt match {

      case (0) =>
        // Print current time in ms
        Utils.outln(s"$getName: Current time in ms = ${System.currentTimeMillis()}")

      case (0x0e) =>
        //   Reset the SIM Pseudo Device
        // Nothing to do

      case 0x12 =>
        // Determine if there is banked memory
        // TODO

      case (22) =>
        // Stop timer Interrupts
        // TODO

      case (27) =>
        // SimH Sleep - Don't bother, system resolution is rarely good enough for nano times.
        Thread.sleep(0,500)

      case _ => {}
    }
    UByte(0x00)
  }

  private def simh_in(byte: UByte): UByte = {
    //Utils.outln(s"Read 0xfe - $byte")

    UByte(0x00)
  }
}
