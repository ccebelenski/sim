package com.sim

import java.awt.EventQueue
import java.awt.event.{KeyEvent, KeyListener}
import javax.swing.JFrame

import com.jterminal.JTerminal


/**
  * Created by christophercebelenski on 7/1/16.
  */
class App {

}

object App {
  def main(args: Array[String]): Unit = {

    var con: Console = null

    EventQueue.invokeLater(new Runnable() {
      override def run(): Unit = {
        con = new Console()
        con.setVisible(true)
      }
    })

  }
}

class Console extends JFrame {

  initUI()


  private def initUI(): Unit = {
    setTitle("SIM")
    setSize(800, 600)
    setLocationRelativeTo(null)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    addKeyListener(new SimKeyListener)

    Console.term = new JTerminal()
    add(Console.term)
    Console.term.println("SIM 0.1")
  }


}

object Console {
  var term: JTerminal = _
}

class SimKeyListener extends KeyListener {
  override def keyTyped(e: KeyEvent): Unit = {

    val char = e.getKeyChar
    Console.term.print(char.toString)
    Console.term.repaint()

  }

  override def keyPressed(e: KeyEvent): Unit = {

  }

  override def keyReleased(e: KeyEvent): Unit = {}
}