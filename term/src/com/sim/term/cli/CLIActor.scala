package com.sim.term.cli

import java.awt.event.KeyEvent

import akka.actor.Actor

class CLIActor  extends Actor {

  private val CSICHAR = 0x7f

  override def receive: Receive = {
    case keyEvent:KeyEvent =>
      if(keyEvent.isActionKey) {
        processActionKey(keyEvent)
        System.out.println(KeyEvent.getKeyText(keyEvent.getExtendedKeyCode))
      } else {
        processCharacterKey(keyEvent)
      }

  }

  private def processActionKey(keyEvent:KeyEvent):Unit = {
    if(!CLIMonitor.acceptInput) return // not accepting characters
  }

  private def processCharacterKey(keyEvent:KeyEvent): Unit = {
    if(!CLIMonitor.acceptInput) return // not accepting characters
    keyEvent.getKeyChar match {
      case '\n' => // Return key

        System.out.println("RET")
        CLIMonitor.cmdLine = "Test"
        CLIMonitor.doNotify()

      case x:Char => // Other key
        System.out.print(x)
    }


  }
}
