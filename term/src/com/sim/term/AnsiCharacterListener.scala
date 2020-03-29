package com.sim.term

import java.awt.event.{KeyEvent, KeyListener}

class AnsiCharacterListener(val model: AbstractTerminalModel) extends KeyListener{
  override def keyTyped(e: KeyEvent): Unit = {
    System.out.println(e.getKeyChar)
    model.print("Key:" + e.getKeyChar + "")

  }

  override def keyPressed(e: KeyEvent): Unit = {}

  override def keyReleased(e: KeyEvent): Unit = {}

}
