package com.sim.term

import java.awt.event.{KeyEvent, KeyListener}

import akka.actor.ActorRef

class CharacterListener(val model: AbstractTerminalModel, val actor:ActorRef ) extends KeyListener{
  override def keyTyped(e: KeyEvent): Unit = {
    //System.out.println(e.getExtendedKeyCode)
    //System.out.println(e.getKeyChar)
    //actor ! e.getKeyChar

  }

  override def keyPressed(e: KeyEvent): Unit = {
    actor ! e
  }

  override def keyReleased(e: KeyEvent): Unit = {}

}
