package com.sim.term

import java.awt.event.{KeyEvent, KeyListener}

import akka.actor.ActorRef
import com.sim.term.cli.CLIMonitor
import javax.swing.JComponent

class CharacterListener(val model: AbstractTerminalModel, parent: JComponent) extends KeyListener {

  val cmdBuffer = new StringBuilder
  var cmdPosition = 0

  private val ESC = 0x1b.toChar
  private val CSI = ESC + "["

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {

    if (!CLIMonitor.acceptInput) return
    if (e.isActionKey) {
      e.getKeyCode match {

        case KeyEvent.VK_LEFT if (cmdPosition > 0) =>
          // Output ANSI sequence to move cursor left, change our cmdPosition
          cmdPosition = cmdPosition - 1
          model.print(CSI + "D")
          parent.repaint()

        case KeyEvent.VK_RIGHT if (cmdPosition != cmdBuffer.length()) =>
          cmdPosition += 1
          model.print(CSI + "C")
          parent.repaint()

        case KeyEvent.VK_END if(cmdPosition > 0) =>
          model.print(CSI + s"${cmdBuffer.length() - cmdPosition}C")
          cmdPosition = cmdBuffer.length()
          parent.repaint()

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
        case '\b' if (cmdPosition > 0) =>
          //model.print("\b")
          //cmdPosition = cmdPosition - 1
          cmdBuffer.deleteCharAt(cmdPosition -1 )
          model.print(CSI + "D" + CSI + "K")
          model.print(cmdBuffer.substring(cmdPosition -1))
          model.print(CSI + s"${model.getCursorColumn - cmdPosition - 1}D")
          cmdPosition = cmdPosition - 1

          parent.repaint()
        case 0x03 =>
          model.print("^C\r\n")
          parent.repaint()
          cmdPosition = 0
          cmdBuffer.clear()
          CLIMonitor.cmdLine = cmdBuffer.toString()
          CLIMonitor.doNotify()
        case kc: Char if (!kc.isControl && kc.toByte != -1) =>
          model.print(kc.toString)
          cmdBuffer.insert(cmdPosition, kc)
          cmdPosition += 1
          model.print(CSI + "D" + CSI + "K")
          model.print(cmdBuffer.substring(cmdPosition -1 ))
          model.print(CSI + s"${model.getCursorColumn - cmdPosition -2 }D")

          parent.repaint()
        case _ => // Anything else we don't care
      }
    }
  }

  override def keyReleased(e: KeyEvent): Unit = {}

}
