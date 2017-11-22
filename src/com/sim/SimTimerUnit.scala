package com.sim

import com.sim.device.BasicUnit

class SimTimerUnit extends BasicUnit {

  var rtc_ticks: Int = 0
  /* ticks */
  var rtc_hz = 0
  /* tick rate */
  var rtc_last_hz = 0
  /* prior tick rate */
  var rtc_rtime = 0
  /* real time */
  var rtc_vtime = 0
  /* virtual time */
  var rtc_gtime = 0
  /* instruction time */
  var rtc_nxintv = 0
  /* next interval */
  var rtc_based = 0
  /* base delay */
  var rtc_currd = 0
  /* current delay */
  var rtc_initd = 0
  /* initial delay */
  var rtc_elapsed = 0
  /* sec since init */
  var rtc_calibrations = 0
  /* calibration count */
  var rtc_clock_skew_max = 0
  /* asynchronous max skew */
  var rtc_clock_start_gtime = 0
  /* reference instruction time for clock */
  var rtc_clock_tick_size = 0
  /* 1/hz */
  var rtc_calib_initializations = 0
  /* Initialization Count */
  var rtc_calib_tick_time = 0
  /* ticks time */
  var rtc_calib_tick_time_tot = 0
  /* ticks time - total*/
  var rtc_calib_ticks_acked = 0
  /* ticks Acked */
  var rtc_calib_ticks_acked_tot = 0
  /* ticks Acked - total */
  var rtc_clock_ticks = 0
  /* ticks delivered since catchup base */
  var rtc_clock_ticks_tot = 0
  /* ticks delivered since catchup base - total */
  var rtc_clock_init_base_time = 0
  /* reference time for clock initialization */
  var rtc_clock_tick_start_time = 0
  /* reference time when ticking started */
  var rtc_clock_catchup_base_time = 0
  /* reference time for catchup ticks */
  var rtc_clock_catchup_ticks = 0
  /* Record of catchups */
  var rtc_clock_catchup_ticks_tot = 0
  /* Record of catchups - total */
  var rtc_clock_catchup_pending = false
  /* clock tick catchup pending */
  var rtc_clock_catchup_eligible = false
  /* clock tick catchup eligible */
  var rtc_clock_time_idled = 0
  /* total time idled */
  var rtc_clock_time_idled_last = 0
  /* total time idled */
  var rtc_clock_calib_skip_idle = 0
  /* Calibrations skipped due to idling */
  var rtc_clock_calib_gap2big = 0
  /* Calibrations skipped Gap Too Big */
  var rtc_clock_calib_backwards = 0
  /* Calibrations skipped Clock Running Backwards */
  var sim_idle_cyc_ms = 0; /* Cycles per millisecond while not idling */


}
