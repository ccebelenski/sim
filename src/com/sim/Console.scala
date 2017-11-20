package com.sim

import java.awt.event.{KeyEvent, KeyListener}
import javax.swing.JFrame

import com.jterminal.JTerminal
import org.beryx.textio.{TextIO, TextIoFactory, TextTerminal}

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

    version.process(null)
  }

  private def readCommand(): String = {
    val input = textIO.newStringInputReader().read("Sim> ")
    input
  }

  private def commandLoop() : Unit = {
    var exiting:Boolean = false
    while(!exiting) {
      val cmd = readCommand
      exiting = evalCommand(cmd.trim)
    }
  }

  private def evalCommand(cmd:String) : Boolean = {

    if(cmd == null || cmd.isEmpty) return false
    val cmdTokenList = cmd.toUpperCase.split(' ')

    Console.commandTree.find(_._2.commandMatch(cmdTokenList(0))).foreach(_._2.process(cmdTokenList.slice(1,cmdTokenList.size)))

    false

  }
}

object Console {
  var term: JTerminal = _
  var textTerminal : TextTerminal[_] = _

  val commandTree : mutable.HashMap[String, Command] = new mutable.HashMap[String,Command]()

}
