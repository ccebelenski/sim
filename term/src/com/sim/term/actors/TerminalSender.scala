package com.sim.term.actors

import java.awt.event.KeyEvent

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

// Braindead load for characters - logs them out only.  Useful for testing, but otherwise
// you're going to want to create your own receiving actor
class TerminalSender extends Actor {

  override def receive: Receive = {
    case x:KeyEvent =>
      if(x.isActionKey) {
        System.out.println(KeyEvent.getKeyText(x.getExtendedKeyCode))
      } else {
        System.out.print(x.getKeyChar)
      }

  }
}
