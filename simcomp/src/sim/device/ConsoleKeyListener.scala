package sim.device

import java.awt.event.{KeyEvent, KeyListener}

import com.sim.term.AbstractTerminalModel
import sim.Utils

class ConsoleKeyListener(val unit: ConsoleUnit) extends KeyListener {

  private val ESC: Byte = 0x1b
  private val CSI = ESC + "["

  override def keyTyped(e: KeyEvent): Unit = {
    if (!e.isActionKey && e.getKeyChar != '\n' && e.getKeyChar != "\r") {

      unit.inputBuffer.enqueue(e.getKeyChar.toByte)
      unit.inputCharacterWaiting = true
    }
  }


  override def keyPressed(e: KeyEvent): Unit = {
    if (e.isActionKey) {
      e.getKeyCode match {

        case KeyEvent.VK_ENTER =>
          unit.inputBuffer.enqueue(13)
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_LEFT =>
          // Output ANSI sequence to move cursor left
          //model.print(CSI + "D")
          unit.inputBuffer.enqueue(ESC)
          unit.inputBuffer.enqueue('[')
          unit.inputBuffer.enqueue('D')
          unit.inputCharacterWaiting = true

        case KeyEvent.VK_RIGHT =>
        //model.print(CSI + "C")


        case KeyEvent.VK_END =>

        case KeyEvent.VK_HOME =>

        case KeyEvent.VK_UP =>

        case KeyEvent.VK_DOWN =>

        case _ => // Skip everything else
      }

    } else {
      if (e.isControlDown && e.getKeyCode == 77) {
        unit.inputBuffer.enqueue('\r')
        unit.inputCharacterWaiting = true
      }
      else if (e.isControlDown && e.getKeyCode == 74) {
        unit.inputBuffer.enqueue('\n')
        unit.inputCharacterWaiting = true
      } else if (e.getKeyCode == KeyEvent.VK_ENTER) {
        unit.inputBuffer.enqueue('\r')
        unit.inputCharacterWaiting = true

      }
    }
    //    if (!e.isControlDown && e.getExtendedKeyCode == 10) {
    //      unit.inputBuffer.enqueue('\r')
    //      unit.inputCharacterWaiting = true
    //    } else if(e.isControlDown && e.getExtendedKeyCode == 10) {
    //      unit.inputBuffer.enqueue('\n')
    //      unit.inputCharacterWaiting = true
    //    }
  }

  override def keyReleased(e: KeyEvent): Unit = {}
}
