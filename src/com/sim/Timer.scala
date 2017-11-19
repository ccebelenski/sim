package com.sim

import org.slf4j.{Logger, LoggerFactory}


/**
  * Created by christophercebelenski on 7/18/16.
  */
class Timer {

  val logger: Logger = LoggerFactory.getLogger(classOf[Timer])
  // timers
  val SIM_NTIMERS = 8
  // Max timer makeup
  val SIM_TMAX = 500
  val SIM_INIITAL_IPS = 50000
  // uncalibrated assumptions about instructions per second
  val SIM_IDLE_CAL = 10
  // ms to calibrate
  val SIM_IDLE_MAX = 10
  // max granularity idle
  val SIM_IDLE_STMIN = 2
  // min sec for stability
  val SIM_IDLE_STDFLT = 20
  /* dft sec for stability */
  val SIM_IDLE_STMAX = 600                         /* max sec for stability */

  val SIM_THROT_WINIT = 1000
  /* cycles to skip */
  val SIM_THROT_WST = 10000
  /* initial wait */
  val SIM_THROT_WMUL = 4
  /* multiplier */
  val SIM_THROT_WMIN = 100
  /* min wait */
  val SIM_THROT_MSMIN = 10
  /* min for measurement */
  val SIM_THROT_NONE = 0
  /* throttle parameters */
  val SIM_THROT_MCYC = 1
  /* MegaCycles Per Sec */
  val SIM_THROT_KCYC = 2
  /* KiloCycles Per Sec */
  val SIM_THROT_PCT = 3
  /* Max Percent of host CPU */
  val SIM_THROT_SPC = 4 /* Specific periodic Delay */

  /**
    * Get Minimum sleep time available for host.
    *
    * @return
    */
  def timerInit(): Boolean = {

    logger.debug("timerInit()")


    true
  }
}
