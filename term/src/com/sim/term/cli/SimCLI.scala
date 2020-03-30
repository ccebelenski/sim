package com.sim.term.cli

import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import com.sim.term.{Term, VT100TerminalModel}
import javax.swing.{JFrame, WindowConstants}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
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
  }

  // Get a command line string
  def getline: String = {

    t.print(prompt.getOrElse(""))
    val future :Future[String] = ask(actor,JunkObject)(Timeout(1,TimeUnit.MINUTES)).mapTo[String]
    val cmdLine = Await.result(future, 1 minute)

    cmdLine
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