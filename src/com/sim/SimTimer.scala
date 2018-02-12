package com.sim

import com.sim.device.{BasicDevice, BasicUnit}
import com.sim.machine.AbstractMachine


class SimTimer(override val machine: AbstractMachine) extends BasicDevice(machine) {

  override val description: String = "Timer Device"
  override val name = "TIMER"

  init()

  /* sim_show_timers - show running timer information */
  def showCommand(sb: StringBuilder) :Unit = {
      val inst_per_sec = SimTimer.internal_timer.sim_timer_inst_per_sec()

      sb.append(f"Minimum Host Sleep Time:       ${SimTimer.OSSleepMin_ms}%d ms (${SimTimer.sim_os_tick_hz}%dHz)\n")
      if (SimTimer.OSSleepMin_ms != SimTimer.OSSleepInc_ms)
        sb.append(f"Minimum Host Sleep Incr Time:  ${SimTimer.OSSleepInc_ms}%d ms\n")
      sb.append(f"Host Clock Resolution:         ${SimTimer.sim_os_clock_resolution_ms}%d ms\n")
      sb.append(f"Execution Rate:                ${inst_per_sec}%d cycles/sec\n")
      if (SimTimer.sim_idle_enab) {
        sb.append("Idling:                        Enabled\n")
        sb.append(f"Time before Idling starts:     ${SimTimer.sim_idle_stable}%d seconds\n")
      }
      if (SimTimer.sim_throt_type != SimTimer.SIM_THROT_NONE) {
        // TODO sim_show_throt ( NULL, uptr, val, desc)
      }
      sb.append("Calibrated Timer:              Internal Timer\n")
      sb.append(s"Catchup Ticks:                 ${if(SimTimer.sim_catchup_ticks) "Enabled" else "Disabled"} for clocks ticking faster than ${SimTimer.sim_os_tick_hz} Hz\n")
      if (SimTimer.sim_idle_calib_pct == 0)
        sb.append("Calibration:                   Always\n")
      else
        sb.append(f"Calibration:                   Skipped when Idle exceeds ${SimTimer.sim_idle_calib_pct}%d%%\n")
      sb.append("\n")
      units.foreach(t => {
        val tmr = t.asInstanceOf[SimTimerUnit]
        sb.append(s"  Clock device is ${tmr.getName()} ${if(tmr == SimTimer.internal_timer) "Internal Calibrated Timer"}\n")
        sb.append(s"${if(SimTimer.sim_asynch_timer) "Asynchronus" else if(tmr.rtc_hz != 0) "Calibrated" else "Uncalibrated"} Timer:\n")
        if (tmr.rtc_hz != 0) {
          sb.append(f"  Running at:                ${tmr.rtc_hz}%d Hz\n")
          sb.append(f"  Tick Size:                 ${tmr.rtc_clock_tick_size}%s\n")
          sb.append(f"  Ticks in current second:   ${tmr.rtc_ticks}%d\n")
        }
        sb.append(s"  Seconds Running:           ${tmr.rtc_elapsed} (%s)\n")
        if (tmr ==SimTimer.internal_timer) {
          sb.append( s"  Calibration Opportunities: ${tmr.rtc_calibrations}\n")
          if (SimTimer.sim_idle_calib_pct != 0)
            sb.append( s"  Calib Skip Idle Thresh %%:  ${SimTimer.sim_idle_calib_pct}\n")
          if (tmr.rtc_clock_calib_skip_idle != 0)
            sb.append( s"  Calibs Skip While Idle:    ${tmr.rtc_clock_calib_skip_idle}\n")
          //if (tmr.rtc_clock_calib_backwards != 0)
          //sb.append( s"  Calibs Skip Backwards:     ${tmr.rtc_clock_calib_backwards}\n")
          if (tmr.rtc_clock_calib_gap2big != 0)
            sb.append( s"  Calibs Skip Gap Too Big:   ${tmr.rtc_clock_calib_gap2big}\n")
        }
        if (tmr.rtc_gtime != 0)
          sb.append( s"  Instruction Time:          ${tmr.rtc_gtime}\n")
        if ((!SimTimer.sim_asynch_timer) && (SimTimer.sim_throt_type == SimTimer.SIM_THROT_NONE)) {
          sb.append( s"  Real Time:                 ${tmr.rtc_rtime}\n")
          sb.append( s"  Virtual Time:              ${tmr.rtc_vtime}\n")
          sb.append( s"  Next Interval:             ${tmr.rtc_nxintv}\n")
          sb.append( s"  Base Tick Delay:           ${tmr.rtc_based}\n")
          sb.append( s"  Initial Insts Per Tick:    ${tmr.rtc_initd}\n")
        }
        sb.append( s"  Current Insts Per Tick:    ${tmr.rtc_currd}\n")
        sb.append( s"  Initializations:           ${tmr.rtc_calib_initializations}\n")
        sb.append( s"  Ticks:                     ${tmr.rtc_clock_ticks}\n")
        if (tmr.rtc_clock_ticks_tot+tmr.rtc_clock_ticks != tmr.rtc_clock_ticks)
          sb.append( s"  Total Ticks:               ${tmr.rtc_clock_ticks_tot+tmr.rtc_clock_ticks}\n")
        if (tmr.rtc_clock_skew_max!= 0.0)
          sb.append( s"  Peak Clock Skew:           ${tmr.rtc_clock_skew_max} ${if(tmr.rtc_clock_skew_max < 0) "fast" else "slow"}\n")
        if (tmr.rtc_calib_ticks_acked != 0)
          sb.append( s"  Ticks Acked:               ${tmr.rtc_calib_ticks_acked}\n")
        if (tmr.rtc_calib_ticks_acked_tot+tmr.rtc_calib_ticks_acked != tmr.rtc_calib_ticks_acked)
          sb.append( s"  Total Ticks Acked:         ${tmr.rtc_calib_ticks_acked_tot+tmr.rtc_calib_ticks_acked}\n")
        if (tmr.rtc_calib_tick_time != 0)
          sb.append( s"  Tick Time:                 ${tmr.rtc_calib_tick_time}\n")
        if (tmr.rtc_calib_tick_time_tot+tmr.rtc_calib_tick_time != tmr.rtc_calib_tick_time)
          sb.append( s"  Total Tick Time:           ${tmr.rtc_calib_tick_time_tot+tmr.rtc_calib_tick_time}\n")
        if (tmr.rtc_clock_catchup_ticks != 0)
          sb.append( s"  Catchup Ticks Sched:       ${tmr.rtc_clock_catchup_ticks}\n")
        if (tmr.rtc_clock_catchup_ticks_tot+tmr.rtc_clock_catchup_ticks != tmr.rtc_clock_catchup_ticks)
          sb.append( s"  Total Catchup Ticks Sched: ${tmr.rtc_clock_catchup_ticks_tot +tmr.rtc_clock_catchup_ticks }\n")

        if (tmr.rtc_clock_init_base_time != 0) {
          sb.append( s"  Initialize Base Time:      ${tmr.rtc_clock_init_base_time}\n")
        }
        if (tmr.rtc_clock_tick_start_time != 0) {
          sb.append( s"  Tick Start Time:           ${tmr.rtc_clock_tick_start_time}\n")
        }
        sb.append( s"  Wall Clock Time Now:       ${System.currentTimeMillis()}\n")
        if (tmr.rtc_clock_catchup_eligible) {
          sb.append( s"  Catchup Tick Time:         ${tmr.rtc_clock_catchup_base_time+tmr.rtc_calib_tick_time}\n")
          sb.append( s"  Catchup Base Time:         ${tmr.rtc_clock_catchup_base_time}\n")
        }
        if (tmr.rtc_clock_time_idled != 0)
          sb.append( s"  Total Time Idled:          ${tmr.rtc_clock_time_idled/1000}\n")
      })


    }


  override def createDefaultUnitOptions: Unit = {} // No options yet

  override def init(): Unit =
  {

    var clock_start : Long = 0L
    var clock_last : Long = 0L
    var clock_now : Long = 0L

    //sim_debug (DBG_TRC, &sim_timer_dev, "sim_timer_init()\n");
    //sim_stop_unit.action = &sim_timer_stop_svc;
    //sim_throttle_unit.action = &sim_throt_svc;
    SimTimer.sim_idle_enab = false                                  /* init idle off */
    SimTimer.sim_idle_rate_ms = SimTimer.computeMinimumSleep()             /* get OS timer rate */
    SimTimer.setROMDelayFactor(SimTimer.getROMDelayFactor); /* initialize ROM delay factor */

    clock_last = System.currentTimeMillis()
      clock_start = clock_last
    SimTimer.sim_os_clock_resolution_ms = 1000
    do {
      var clock_diff:Long =0L

      clock_now =System.currentTimeMillis()
      clock_diff = clock_now - clock_last
      if ((clock_diff > 0) && (clock_diff < SimTimer.sim_os_clock_resolution_ms))
        SimTimer.sim_os_clock_resolution_ms = clock_diff
      clock_last = clock_now
    } while (clock_now < clock_start + 100)
    SimTimer.sim_os_tick_hz = 1000/(SimTimer.sim_os_clock_resolution_ms * (SimTimer.sim_idle_rate_ms/SimTimer.sim_os_clock_resolution_ms))
  }
}

object SimTimer {

  val TMAX = 500 // max timer makeup
  val INITIAL_IPS = 500000 // uncalibrated assumption about instructions per second
  val IDLE_CAL = 10 // ms to calibrate
  val IDLE_STMIN = 2 // min sec for stability
  val IDLE_STDFLT = 20 // dft sec for stability
  val IDLE_STMAX = 600 // max sec for stability
  val CLK_TPS = 10 // 10Hz system clock for internal timer

  /* throttle parameters */
  val SIM_THROT_NONE = 0
  /* MegaCycles Per Sec */
  val SIM_THROT_MCYC = 1
  /* KiloCycles Per Sec */
  val SIM_THROT_KCYC = 2
  /* Max Percent of host CPU */
  val SIM_THROT_PCT = 3
  /* Specific periodic Delay */
  val SIM_THROT_SPC = 4

  /* ms to calibrate */
  val SIM_IDLE_CAL = 10
  /* min sec for stability */
  val SIM_IDLE_STMIN = 2
  /* dft sec for stability */
  val SIM_IDLE_STDFLT = 20
  /* max sec for stability */
  val SIM_IDLE_STMAX = 600

  var OSSleepMin_ms: Long = 0L
  var OSSleepInc_ms: Long = 0L
  var sim_time: Long = 0L
  var sim_rtime: Long = 0L
  var sim_interval: Long = 0L
  var noqueue_time: Long = 0L


  // Internal calibrated Timer
  var internal_timer: SimTimerUnit = _

  var sim_os_clock_resolution_ms = 1L
  var sim_internal_clock_tps = 0L
  var sim_os_tick_hz = 0L

  var sim_idle_rate_ms = 0L
  var sim_rom_delay = 0L
  var sim_idle_calib_pct = 0L
  var sim_idle_stable :Int = SIM_IDLE_STDFLT
  var sim_catchup_ticks:Boolean = true

  var sim_inst_per_sec_last:Long = 0L


  var sim_asynch_timer: Boolean = false
  var sim_throt_type: Int = SIM_THROT_NONE
  var sim_idle_enab: Boolean = false

  def sim_timer_init(): Boolean = {
    var clock_start = 0L
    var clock_last = 0L
    var clock_now = 0L

    //for (tmr=0; tmr<=SIM_NTIMERS; tmr++) {
    //sim_timer_units[tmr].action = &sim_timer_tick_svc;
    //sim_timer_units[tmr].flags = UNIT_DIS | UNIT_IDLE;
    //sim_clock_cosched_queue[tmr] = QUEUE_LIST_END;
    //}
    //sim_stop_unit.action = &sim_timer_stop_svc;
    //SIM_INTERNAL_UNIT.flags = UNIT_IDLE;
    //sim_register_internal_device (&sim_timer_dev);          /* Register Clock Assist device */
    //sim_throttle_unit.action = &sim_throt_svc;
    //sim_register_clock_unit_tmr (&SIM_INTERNAL_UNIT, SIM_INTERNAL_CLK);
    //sim_idle_enab = FALSE;                                  /* init idle off */
    sim_idle_rate_ms = SimTimer.computeMinimumSleep() /* get OS timer rate */
    getROMDelayFactor /* initialize ROM delay factor */

    clock_last = System.currentTimeMillis()
    clock_start = clock_last
    sim_os_clock_resolution_ms = 1000L
    do {
      var clock_diff = 0L

      clock_now = System.currentTimeMillis()
      clock_diff = clock_now - clock_last
      if ((clock_diff > 0) && (clock_diff < sim_os_clock_resolution_ms))
        sim_os_clock_resolution_ms = clock_diff
      clock_last = clock_now
    } while (clock_now < clock_start + 100)
    sim_os_tick_hz = 1000 / (sim_os_clock_resolution_ms * (sim_idle_rate_ms / sim_os_clock_resolution_ms))
    sim_idle_rate_ms != 0
  }

  private def rom_swapb(v: Long): Long = {
    ((v << 24) & 0xff000000) | ((v << 8) & 0xff0000) |
      ((v >> 8) & 0xff00) | ((v >> 24) & 0xff)
  }

  private def getROMDelayFactor: Long = {

    if (sim_rom_delay == 0) {
      /* Calibrate the loop delay factor at startup.
   Do this 4 times and use the largest value computed.
   The goal here is to come up with a delay factor which will throttle
   a 6 byte delay loop running from ROM address space to execute
   1 instruction per usec */
      var te: Long = 0L
      var ts: Long = 0L
      var c = 10000
      var i: Long = 0L
      var samples: Long = 0L
      var rom_loopval: Long = 0L
      var break = false
      while (!break) {
        c = c * 2
        te = System.currentTimeMillis()
        ts = te

        while ((i < c)) {
          i += 1
          rom_loopval = rom_loopval | rom_loopval + ts ^ rom_swapb(rom_swapb(rom_loopval + ts))
        }
        te = System.currentTimeMillis()
        if ((te - ts) > 50) {
          if (sim_rom_delay < (rom_loopval + (c / (te - ts) / 1000) + 1))
            sim_rom_delay = rom_loopval + (c / (te - ts) / 1000) + 1
          samples += 1
          if (samples >= 4) break = true else c = c / 2
        }

      }
      if (sim_rom_delay < 5) sim_rom_delay = 5
    }

    sim_rom_delay
  }

  def setROMDelayFactor(delay:Long ) :Unit  = {
    sim_rom_delay = delay
  }


  def timerActivateAfter(unit: BasicUnit, usecs: Long) = ???

  // TODO from sim_timer.c
  def _sim_timer_activate_time(unit:BasicUnit) :Long = {

    0L
  }

  def computeMinimumSleep(): Long = {
    val sleepSamples = 100
    var i: Int = 0
    var tot: Long = 0L
    var tim: Long = 0L

    val currentPriority = Thread.currentThread().getPriority
    // Update our priority to get some more accurate numbers
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    idleMsSleep(1L) // Start sampling on a tick boundary
    for (i <- 0 to sleepSamples) tot += idleMsSleep(1L)
    tim = tot / sleepSamples

    SimTimer.OSSleepMin_ms = tim

    idleMsSleep(1L) // Start samplng on a tick boundary
    tot = 0L
    for (i <- 0 to sleepSamples) tot += idleMsSleep(SimTimer.OSSleepMin_ms + 1L)
    tim = tot / sleepSamples
    SimTimer.OSSleepInc_ms = tim - SimTimer.OSSleepMin_ms

    Thread.currentThread().setPriority(currentPriority)
    SimTimer.OSSleepMin_ms
  }

  def idleMsSleep(msec: Long): Long = {

    val now = System.currentTimeMillis()

    Thread.sleep(msec.longValue)

    val end = System.currentTimeMillis()
    end - now

  }

  // 443
  def msSleep(msec: Long): Long = idleMsSleep(msec)

  /* sim_timer_idle_capable - tell if the host is Idle capable and what the host OS tick size is */

  def sim_timer_idle_capable(): Boolean = {
    sim_idle_rate_ms != 0
  }

  def sim_timer_inst_per_sec(): Long = {
    var inst_per_sec = sim_inst_per_sec_last
    if(SimTimer.internal_timer == null) return inst_per_sec
    inst_per_sec = SimTimer.internal_timer.rtc_currd * SimTimer.internal_timer.rtc_hz
    if(inst_per_sec == 0) inst_per_sec = SimTimer.internal_timer.rtc_currd * SimTimer.sim_internal_clock_tps
    inst_per_sec
  }

  def updateSimTime(): Unit = {
    var _x: Long = 0
    val queueEmpty = EventQueue.clockQueue.isEmpty
    if (queueEmpty) _x = noqueue_time
    else _x = EventQueue.clockQueue.head.time

    sim_time = sim_time + (_x - sim_interval)
    sim_rtime = sim_rtime + (_x - sim_interval)
    if (queueEmpty) noqueue_time = sim_interval
    else EventQueue.clockQueue.head.time = sim_interval
  }

}