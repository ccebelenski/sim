package sim.timer

import sim.SimTimer
import org.junit.Test

class TimerTests {

  @Test
  def testTimerMinimumSleep() : Unit = {
    val result = SimTimer.computeMinimumSleep()

    System.out.println(s"Result = $result")
  }
}
