package com.sim.timer

import com.sim.SimTimer
import org.junit.Test

class TimerTests {

  @Test
  def testTimerMinimumSleep() : Unit = {
    val timer = new SimTimer
    val result = timer.computeMinimumSleep()

    System.out.println(s"Result = $result")
  }
}
