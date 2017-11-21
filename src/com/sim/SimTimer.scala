package com.sim

import com.sim.unsigned.{UInt, ULong}

class SimTimer {


  def timerInit() = ???

  def RTCCInit() = ???

  def RTCCalibrate() = ???

  def idle() = ???

  def msec() = ???

  def sleep() = ???

  def msSleep() = ???

  def idleMsSleep(msec: ULong) : ULong = {

    val now = System.currentTimeMillis()

    Thread.sleep(msec.longValue)

    val end = System.currentTimeMillis()
    ULong(end - now)
  }

  def timespecDiff() = ???

  def timerActivateAfter() = ???

  def timerActivateTime() = ???

  def timerActivateTimeUsecs() = ???

  def ROMReadWithDelay() = ???

  def getROMDelayFactor() = ???

  def setROMDelayFactor() = ???

  def computeMinimumSleep() : ULong = {
    val sleepSamples = 100
    var i:Int = 0
    var tot:ULong = ULong(0)
    var tim:ULong = ULong(0)

    val currentPriority = Thread.currentThread().getPriority
    // Update our priority to get some more accurate numbers
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    idleMsSleep(UInt(1)) // Start sampling on a tick boundary
    for(i <- 0 to sleepSamples) tot += idleMsSleep(UInt(1))
    tim = ULong(tot / sleepSamples)

    SimTimer.OSSleepMin_ms = tim

    idleMsSleep(UInt(1)) // Start samplng on a tick boundary
    tot = ULong(0)
    for(i <- 0 to sleepSamples) tot += idleMsSleep(SimTimer.OSSleepMin_ms + ULong(1))
    tim = ULong(tot / sleepSamples)
    SimTimer.OSSleepInc_ms = tim - SimTimer.OSSleepMin_ms

    Thread.currentThread().setPriority(currentPriority)
    SimTimer.OSSleepMin_ms
  }

}

object SimTimer {

  val NTIMERS = 8 // # Timers
  val TMAX = 500  // max timer makeup
  val INITIAL_IPS = 500000  // uncalibrated assumption about instructions per second
  val IDLE_CAL = 10 // ms to calibrate
  val IDLE_STMIN = 2 // min sec for stability
  val IDLE_STDFLT = 20 // dft sec for stability
  val IDLE_STMAX = 600 // max sec for stability


  var OSSleepMin_ms : ULong = ULong(0)
  var OSSleepInc_ms : ULong = ULong(0)
}