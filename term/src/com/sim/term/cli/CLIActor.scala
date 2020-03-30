package com.sim.term.cli

import java.awt.event.KeyEvent

import akka.actor.Actor

case object JunkObject

class CLIActor  extends Actor {

  private val CSICHAR = 0x7f

  override def receive: Receive = {
    case keyEvent:KeyEvent =>
      if(keyEvent.isActionKey) {
        System.out.println(KeyEvent.getKeyText(keyEvent.getExtendedKeyCode))
      } else {
        System.out.print(keyEvent.getKeyChar)
      }
      sender ! JunkObject
  }
}
