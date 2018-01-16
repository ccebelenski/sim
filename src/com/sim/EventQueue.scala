package com.sim

import com.sim.device.BasicUnit

import scala.collection.mutable

class EventQueue {

  def processEvent() = {
    SimTimer.updateSimTime()

    while(EventQueue.clockQueue.nonEmpty && SimTimer.sim_interval <= 0){

      val unit: BasicUnit  = EventQueue.clockQueue.dequeue()
      unit.time = 0L
      if(EventQueue.clockQueue.nonEmpty)
        SimTimer.sim_interval = EventQueue.clockQueue.front.time
      else {
        SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
        SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
      }
      if(unit.usecs_remaining != 0) SimTimer.timerActivateAfter(unit,unit.usecs_remaining)
      //else unit.action()

    }

    if(EventQueue.clockQueue.isEmpty) {
      SimTimer.sim_interval = EventQueue.NOQUEUE_WAIT
      SimTimer.noqueue_time = EventQueue.NOQUEUE_WAIT
    }
  }


}

object EventQueue {
  val clockQueue : mutable.Queue[BasicUnit] = new mutable.Queue[BasicUnit]()

  val NOQUEUE_WAIT = 1000000L // Min check time
}