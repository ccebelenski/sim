package com.sim

import com.sim.device.BasicDevice
import com.sim.unsigned.{UInt, ULong}

class SimTimer extends BasicDevice {

  override val description: String = "Timer Device"
  val name: String = "TIMER"
  override var deviceName: String = ""

  def init():Unit = ???

  def RTCCInit() = ???

  def RTCCalibrate() = ???

  def idle() = ???

  def msec() = ???

  def sleep() = ???

  // 443
  def msSleep(msec: ULong) : ULong = idleMsSleep(msec)

  def idleMsSleep(msec: ULong): ULong = {

    val now = System.currentTimeMillis()

    Thread.sleep(msec.longValue)

    val end = System.currentTimeMillis()
    ULong(end - now)
  }


  def timerActivateAfter() = ???

  def timerActivateTime() = ???

  def timerActivateTimeUsecs() = ???

  def ROMReadWithDelay() = ???

  def getROMDelayFactor() = ???

  def setROMDelayFactor() = ???

  def computeMinimumSleep(): ULong = {
    val sleepSamples = 100
    var i: Int = 0
    var tot: ULong = ULong(0)
    var tim: ULong = ULong(0)

    val currentPriority = Thread.currentThread().getPriority
    // Update our priority to get some more accurate numbers
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    idleMsSleep(UInt(1)) // Start sampling on a tick boundary
    for (i <- 0 to sleepSamples) tot += idleMsSleep(UInt(1))
    tim = ULong(tot / sleepSamples)

    SimTimer.OSSleepMin_ms = tim

    idleMsSleep(UInt(1)) // Start samplng on a tick boundary
    tot = ULong(0)
    for (i <- 0 to sleepSamples) tot += idleMsSleep(SimTimer.OSSleepMin_ms + ULong(1))
    tim = ULong(tot / sleepSamples)
    SimTimer.OSSleepInc_ms = tim - SimTimer.OSSleepMin_ms

    Thread.currentThread().setPriority(currentPriority)
    SimTimer.OSSleepMin_ms
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

  var OSSleepMin_ms: ULong = ULong(0)
  var OSSleepInc_ms: ULong = ULong(0)
  var sim_time : ULong = ULong(0)

  // Internal calibrated Timer
  var internal_timer: SimTimerUnit =  _

  var sim_os_clock_resolution_ms = 0L
  var sim_internal_clock_tps =0L

  def sim_timer_init () : Boolean  =
  {
    val clock_start, clock_last, clock_now = 0L

    for (tmr=0; tmr<=SIM_NTIMERS; tmr++) {
    sim_timer_units[tmr].action = &sim_timer_tick_svc;
    sim_timer_units[tmr].flags = UNIT_DIS | UNIT_IDLE;
    sim_clock_cosched_queue[tmr] = QUEUE_LIST_END;
  }
    sim_stop_unit.action = &sim_timer_stop_svc;
    SIM_INTERNAL_UNIT.flags = UNIT_IDLE;
    sim_register_internal_device (&sim_timer_dev);          /* Register Clock Assist device */
    sim_throttle_unit.action = &sim_throt_svc;
    sim_register_clock_unit_tmr (&SIM_INTERNAL_UNIT, SIM_INTERNAL_CLK);
    sim_idle_enab = FALSE;                                  /* init idle off */
    sim_idle_rate_ms = sim_os_ms_sleep_init ();             /* get OS timer rate */
    sim_set_rom_delay_factor (sim_get_rom_delay_factor ()); /* initialize ROM delay factor */

    clock_last = clock_start = sim_os_msec ();
    sim_os_clock_resolution_ms = 1000L
    do {
      var clock_diff = 0L

      clock_now = sim_os_msec ();
      clock_diff = clock_now - clock_last;
      if ((clock_diff > 0) && (clock_diff < sim_os_clock_resoluton_ms))
        sim_os_clock_resolution_ms = clock_diff;
      clock_last = clock_now;
    } while (clock_now < clock_start + 100);
    sim_os_tick_hz = 1000/(sim_os_clock_resoluton_ms * (sim_idle_rate_ms/sim_os_clock_resoluton_ms));
    return (sim_idle_rate_ms != 0);
  }

}