package com.sim.term.cli

import akka.actor.{ActorSystem, Props}
import com.sim.term.{Term, VT100TerminalModel}
import javax.swing.{JFrame, WindowConstants}

import scala.language.postfixOps


class SimCLI {

  private val system = ActorSystem("CLI")
  private val actor = system.actorOf(Props[CLIActor], name = "CLIActor")

  private val frame = new JFrame()
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  private val t = new Term(new VT100TerminalModel(80, 24), actor)
  frame.add(t)
  frame.pack()
  frame.setVisible(true)

  // Buffer to store the cmd line
  private val buffer: StringBuilder = new StringBuilder
  private var prompt: Option[String] = None

  // Set a prompt
  def setPrompt(prompt: String): Unit = {
    this.prompt = Some(prompt)
    CLIMonitor.promptLength = prompt.length

  }

  // Get a command line string
  def getline: String = {

    CLIMonitor.acceptInput = true
    t.print(prompt.getOrElse(""))

    CLIMonitor.waitForLine()
    CLIMonitor.acceptInput = false

    CLIMonitor.cmdLine
  }
}


// Test object - starts a terminal windows and takes in input
object SimCLI {

  // Main method for testing
  def main(args: Array[String]): Unit = {

    val x = new SimCLI
    x.setPrompt("> ")
    System.out.println("\n\n\nResult>" + x.getline)

  }
}

private class CLIMonitor

object CLIMonitor {
  @volatile
  var cmdLine: String = _

  @volatile
  var acceptInput: Boolean = true

  var promptLength : Int = 0

  private val monitor: CLIMonitor = new CLIMonitor()

  def waitForLine(): Unit = {
    monitor.synchronized {
      monitor.wait()
    }
  }

  def doNotify(): Unit = {
    monitor.synchronized {
      monitor.notify()
    }
  }
}
