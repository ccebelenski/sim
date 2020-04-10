package sim.device

import java.awt.BorderLayout

import com.sim.term.{Term, VT100TerminalModel}
import javax.swing.{JFrame, WindowConstants}

abstract class ConsoleUnit(device:BasicDevice) extends BasicUnit(device:BasicDevice) {

  val inputBuffer = new scala.collection.mutable.Queue[Byte]

  @volatile
  var inputCharacterWaiting:Boolean = false

   private val frame = new JFrame()
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  private val t = new ConsoleTerminal(new VT100TerminalModel(80, 24), this)
  private val d = t.getSize()
  d.setSize(d.height , d.width )
  frame.setSize(d)
  frame.add(t)
  frame.add(BorderLayout.CENTER, t)
  frame.pack()
  frame.setTitle(getName)
  frame.setVisible(true)

  override def cancel(): Unit = ???

  override def completeAction(): Unit = {
    if (inputCharacterWaiting) {
      device.machine.getCPU.keyboardInterrupt = true
    }

  }

  def getTerminal:ConsoleTerminal = t


  override def init(): Unit = {
    //as each unit is created, we need a console and a keylistener that console.
    // TODO
  }

  override def optionChanged(sb: StringBuilder): Unit = ???
}
