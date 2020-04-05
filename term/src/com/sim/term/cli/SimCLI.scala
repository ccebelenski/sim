package com.sim.term.cli

import akka.actor.{ActorSystem, Props}
import com.sim.term.{Term, VT100TerminalModel}
import javax.swing.{JFrame, WindowConstants}

import scala.language.postfixOps


class SimCLI {

  private val frame = new JFrame()
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  private val t = new Term(new VT100TerminalModel(80, 24))
  private val d = t.getSize()
  d.setSize(d.height + 500, d.width + 500)
  frame.setSize(d)
  frame.add(t)
  frame.pack()
  frame.setVisible(true)

  private var prompt: Option[String] = None

  // Set a prompt
  def setPrompt(prompt: String): Unit = {
    this.prompt = Some(prompt)
    CLIMonitor.promptLength = prompt.length

  }

  // Get a command line string
  def getline: String = {
    t.print(prompt.getOrElse(""))
    CLIMonitor.waitForLine()
    CLIMonitor.cmdLine
  }

  def getTerm: Term = t
}


// Test object - starts a terminal windows and takes in input
object SimCLI {

  // Main method for testing
  def main(args: Array[String]): Unit = {

    var result:String = null
    val x = new SimCLI
    System.out.println("Enter 'exit' to terminate.")
    x.setPrompt("sim> ")
    while(result != "exit") {
      result = x.getline
      System.out.println(s"\nResult> ${result}")
      System.out.println(s"Length: ${result.length}")
    }
  }
}

private class CLIMonitor

object CLIMonitor {
  @volatile
  var cmdLine: String = _

  @volatile
  var acceptInput: Boolean = false

  var promptLength : Int = 0

  private val monitor: CLIMonitor = new CLIMonitor()

  def waitForLine(): Unit = {
    monitor.synchronized {
      acceptInput = true
      monitor.wait()
      acceptInput = false
    }
  }

  def doNotify(): Unit = {
    monitor.synchronized {
      monitor.notify()
    }
  }
}
