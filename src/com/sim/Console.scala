package com.sim

import com.jterminal.JTerminal
import org.beryx.textio.{TextIoFactory, TextTerminal}

import scala.collection.mutable

class Console {

  val textIO = TextIoFactory.getTextIO

  def initUI(): Unit = {

    Console.textTerminal = textIO.getTextTerminal


    // Add the "top level" simulator commands - VERSION, HELP, LOAD, EXIT, SHOW, SET, ATTACH, DETACH
    val version:Command = new VersionCommand
    Console.commandTree.put(version.commandToken, version)

    val help:Command = new HelpCommand
    Console.commandTree.put(help.commandToken, help)

    val list:Command = new ListCommand
    Console.commandTree.put(list.commandToken, list)

    val show:Command = new ShowCommand
    Console.commandTree.put(show.commandToken, show)

    val set:Command = new SetCommand
    Console.commandTree.put(set.commandToken, set)

    val exit:Command = new ExitCommand
    Console.commandTree.put(exit.commandToken, exit)

    version.process(null)
  }

  private def readCommand(): String = {
    val input = textIO.newStringInputReader().read("Sim>")
    input
  }

  def commandLoop() : Unit = {
    var exiting:Boolean = false
    while(!exiting) {
      Console.userInterrupt = false // Reset the interrupt
      val cmd = readCommand()
      exiting = evalCommand(cmd.trim)
    }
  }

  private def evalCommand(cmd:String) : Boolean = {

    if(cmd == null || cmd.isEmpty) return false
    val cmdTokenList = cmd.toUpperCase.split(' ')

    Console.commandTree.find(_._2.commandMatch(cmdTokenList(0))) match {
      case None=>
        Utils.outln("SIM: Invalid command")
        false
      case Some(x) =>
        x._2.process(cmdTokenList.slice(1,cmdTokenList.length))
    }
  }
}

object Console {
  var term: JTerminal = _
  var textTerminal : TextTerminal[_] = _

  val commandTree : mutable.HashMap[String, Command] = new mutable.HashMap[String,Command]()

  val simEnvironment: SimEnvironment = new SimEnvironment

  // This is set by the user interrupt - should be re-set when the command prompt is displayed.
  var userInterrupt : Boolean = false
}
