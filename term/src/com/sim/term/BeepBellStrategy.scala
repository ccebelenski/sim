package com.sim.term

import java.awt.Toolkit

class BeepBellStrategy extends BellStrategy {

  override def soundBell(): Unit = {
    Toolkit.getDefaultToolkit.beep()
  }
}
