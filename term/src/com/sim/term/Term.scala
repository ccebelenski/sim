package com.sim.term

import java.awt.event.{ActionEvent, AdjustmentEvent, KeyEvent, KeyListener}
import java.awt._

import javax.swing._

class Term(val model: AbstractTerminalModel) extends JComponent {

  init()

  /**
    * The unique serial version id.
    */
  private val serialVersionUID = 2871625194146986567L

  /**
    * The scroll bar.
    */
  private var scrollBar: Option[JScrollBar] = None

  /**
    * Initializes the terminal.
    */
  private def init(): Unit = {
    setLayout(new BorderLayout(0, 0))
    val rows = model.getRows
    val bufferSize = model.getBufferSize
    if (bufferSize > rows) {
      scrollBar = Some(new JScrollBar(Adjustable.VERTICAL, 0, rows, 0, bufferSize + 1))
      scrollBar.get.addAdjustmentListener((l) => repaint())
      add(BorderLayout.LINE_END, scrollBar.get)
    }
    val term = new Terminal
    add(BorderLayout.CENTER, term)
    repaint()
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


  // The terminal component
  private class Terminal extends JComponent {

    setDoubleBuffered(true)

    addKeyListener(new AnsiCharacterListener(model))
    setFocusTraversalKeysEnabled(false)
    setFocusable(true)
    requestFocusInWindow()


    //The cell width in pixels.
    private val CELL_WIDTH = 8

    //The cell height in pixels.
    private val CELL_HEIGHT = 12
    // The font.
    val font2: Font = new Font("Monospaced", Font.PLAIN, CELL_HEIGHT)

    // The unique serial version id.
    private val serialVersionUID = 3835622547820767019L

    override def getMinimumSize = new Dimension(model.getColumns * CELL_WIDTH, model.getRows * CELL_HEIGHT)

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


  }

}

object Term {
  // Main method for testing
  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    val t = new Term(new VT100TerminalModel(80, 24))
    frame.add(t)
    frame.pack()
    frame.setVisible(true)

    t.print("Test")
    Thread.sleep(4000)
    t.print("Test2")
  }
}