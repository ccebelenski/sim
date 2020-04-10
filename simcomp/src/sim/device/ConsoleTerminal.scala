package sim.device

import java.awt.{BorderLayout, Dimension, Font, Graphics}

import com.sim.term.{AbstractTerminalModel, CharacterListener, TerminalCell}
import javax.swing.{JComponent, JScrollBar}

class ConsoleTerminal(val model: AbstractTerminalModel, consoleUnit: ConsoleUnit) extends JComponent {

  setDoubleBuffered(true)

  addKeyListener(new ConsoleKeyListener(consoleUnit))
  setFocusTraversalKeysEnabled(false)
  setFocusable(true)
  //add(BorderLayout.CENTER, this)
  repaint()

  //The cell width in pixels.
  private val CELL_WIDTH = 8

  //The cell height in pixels.
  private val CELL_HEIGHT = 12
  // The font.
  val font2: Font = new Font("Monospaced", Font.PLAIN, CELL_HEIGHT)

  /**
    * The scroll bar.
    */
  private var scrollBar: Option[JScrollBar] = None

  override def getMinimumSize = new Dimension(model.getColumns * CELL_WIDTH + 5, model.getBufferSize * CELL_HEIGHT + 5)

  override def getMaximumSize: Dimension = getMinimumSize

  override def getPreferredSize: Dimension = getMinimumSize

  override def paint(g: Graphics): Unit = {
    g.setFont(font2)
    val width = model.getColumns
    val height = model.getBufferSize
    g.setColor(model.getDefaultBackgroundColor)
    g.fillRect(0, 0, width * CELL_WIDTH, height * CELL_HEIGHT)
    val start = scrollBar match {
      case None => 0
      case Some(x) => x.getValue
    }
    for (y <- start until height) {
      for (x <- 0 until width) {
        var cell = model.getCell(x, y)
        val cursorHere = model.getCursorRow == y && model.getCursorColumn == x
        if (cursorHere && cell == null) cell = TerminalCell(' ', model.getDefaultBackgroundColor, model.getDefaultForegroundColor)
        if (cell != null) {
          val px = x * CELL_WIDTH
          val py = (y - start) * CELL_HEIGHT
          g.setColor(if (cursorHere) cell.foregroundColor
          else cell.backgroundColor)
          g.fillRect(px, py, CELL_WIDTH, CELL_HEIGHT)
          g.setColor(if (cursorHere) cell.backgroundColor
          else cell.foregroundColor)
          g.drawChars(Array[Char](cell.character), 0, 1, px, py + CELL_HEIGHT)
        }
      }
    }
  }

  /**
    * Prints a line to the terminal.
    *
    * @param str The string to print.
    */
  def println(str: String): Unit = {
    if (str != null) print(str.concat("\r\n"))
  }

  /**
    * Prints a string to the terminal.
    *
    * @param str The string to print.
    */
  def print(str: String): Unit = {
    model.print(str)
    repaint()
  }



}
