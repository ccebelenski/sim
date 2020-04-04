package com.sim.term

import java.awt.event.{KeyEvent, KeyListener}

import akka.actor.ActorRef
import com.sim.term.cli.CLIMonitor
import javax.swing.JComponent

class CharacterListener(val model: AbstractTerminalModel, parent: JComponent) extends KeyListener{

  val cmdBuffer = new StringBuilder
  var cmdPosition = 0

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {

    if(!CLIMonitor.acceptInput) return
    if(e.isActionKey) {
      e.getKeyCode match {

        case KeyEvent.VK_LEFT =>
          System.out.println("LEFT")

        case KeyEvent.VK_RIGHT =>
          System.out.println("RIGHT")

        case _ => // Skip everything else
      }

    } else {
      e.getKeyChar match {
        case '\n' =>
          model.print("\r\n")
          parent.repaint()
          CLIMonitor.cmdLine = cmdBuffer.toString()
          cmdPosition = 0
          cmdBuffer.clear()
          CLIMonitor.doNotify()
        case '\b' if(cmdPosition > 0) =>
            model.print("\b")
            cmdPosition = cmdPosition - 1
            cmdBuffer.deleteCharAt(cmdPosition )
            parent.repaint()
        case 0x03 =>
          model.print("^C\r\n")
          parent.repaint()
          cmdPosition = 0
          cmdBuffer.clear()
          CLIMonitor.cmdLine = cmdBuffer.toString()
          CLIMonitor.doNotify()
        case kc: Char if(!kc.isControl && kc.toByte != -1) =>
            model.print(kc.toString)
            cmdBuffer.insert(cmdPosition, kc)
            cmdPosition += 1
            System.out.println(kc.toByte)
            parent.repaint()
        case _ => // Anything else we don't care
      }
    }
  }

  override def keyReleased(e: KeyEvent): Unit = {}

}
