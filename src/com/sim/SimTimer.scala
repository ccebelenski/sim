package com.sim

import com.sim.device.{BasicDevice, BasicUnit}
import com.sim.unsigned.{UInt, ULong}

class SimTimer extends BasicDevice {

  override val description: String = "Timer Device"
  override val name: String = "TIMER"

  def init():Unit = ???

  def RTCCInit() = ???

  def RTCCalibrate() = ???

  def idle() = ???

  def msec() = ???

  def sleep() = ???






  def timerActivateTime() = ???

  def timerActivateTimeUsecs() = ???

  def ROMReadWithDelay() = ???





}

object SimTimer {

  val TMAX = 500 // max timer makeup
  val INITIAL_IPS = 500000 // uncalibrated assumption about instructions per second
  val IDLE_CAL = 10 // ms to calibrate
  val IDLE_STMIN = 2 // min sec for stability
  val IDLE_STDFLT = 20 // dft sec for stability
  val IDLE_STMAX = 600 // max sec for stability
  val CLK_TPS = 10 // 10Hz system clock for internal timer

  var OSSleepMin_ms: Long = 0L
  var OSSleepInc_ms: Long = 0L
  var sim_time : Long = 0L
  var sim_rtime: Long = 0L
  var sim_interval: Long = 0L
  var noqueue_time: Long = 0L


  // Internal calibrated Timer
  var internal_timer: SimTimerUnit =  _

  var sim_os_clock_resolution_ms = 1L
  var sim_internal_clock_tps =0L
  var sim_os_tick_hz = 0L

  var sim_idle_rate_ms = 0L
  var sim_set_rom_delay_factor = 0L
  var sim_int_clk_tps = 0L
  var sim_idle_calib_pct = 0L

  def sim_timer_init () : Boolean  =
  {
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
    sim_idle_rate_ms = SimTimer.computeMinimumSleep()           /* get OS timer rate */
    sim_set_rom_delay_factor = getROMDelayFactor(); /* initialize ROM delay factor */

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
    sim_os_tick_hz = 1000/(sim_os_clock_resolution_ms * (sim_idle_rate_ms/sim_os_clock_resolution_ms))
    sim_idle_rate_ms != 0
  }

  def getROMDelayFactor() = ???

  def setROMDelayFactor() = ???


  def timerActivateAfter(unit:BasicUnit, usecs:Long) = ???


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
  def msSleep(msec: Long) : Long = idleMsSleep(msec)

  /* sim_timer_idle_capable - tell if the host is Idle capable and what the host OS tick size is */

  def sim_timer_idle_capable () : Boolean =
  {
    sim_idle_rate_ms != 0
  }


  def updateSimTime() : Unit = {
    var _x: Long = 0
    val queueEmpty = EventQueue.clockQueue.isEmpty
    if(queueEmpty) _x = noqueue_time
    else _x = EventQueue.clockQueue.front.time

    sim_time = sim_time + (_x - sim_interval)
    sim_rtime = sim_rtime + (_x - sim_interval)
    if(queueEmpty) noqueue_time = sim_interval
    else EventQueue.clockQueue.front.time = sim_interval
  }

}