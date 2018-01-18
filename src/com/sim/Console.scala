package com.sim

import com.jterminal.JTerminal
import org.beryx.textio.{TextIoFactory, TextTerminal}

import scala.collection.mutable

class Console {

  val textIO = TextIoFactory.getTextIO

  initUI()

  commandLoop()

  private def initUI(): Unit = {

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

    version.process(null)
  }

  private def readCommand(): String = {
    val input = textIO.newStringInputReader().read("Sim> ")
    input
  }

  private def commandLoop() : Unit = {
    var exiting:Boolean = false
    while(!exiting) {
      val cmd = readCommand()
      exiting = evalCommand(cmd.trim)
    }
  }

  private def evalCommand(cmd:String) : Boolean = {

    if(cmd == null || cmd.isEmpty) return false
    val cmdTokenList = cmd.toUpperCase.split(' ')

    Console.commandTree.find(_._2.commandMatch(cmdTokenList(0))).foreach(_._2.process(cmdTokenList.slice(1,cmdTokenList.length)))

    false

  }
}

object Console {
  var term: JTerminal = _
  var textTerminal : TextTerminal[_] = _

  val commandTree : mutable.HashMap[String, Command] = new mutable.HashMap[String,Command]()

  val simEnvironment: SimEnvironment = new SimEnvironment


}
