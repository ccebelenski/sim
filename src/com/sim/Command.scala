package com.sim

import scala.collection.mutable

abstract class Command {

  private val subCommandTree: mutable.HashMap[String, Command] = new mutable.HashMap[String, Command]()

  // Token this command responds to
  var commandToken: String = _
  var commandDescription: String = _
  var commandHelpText: String = "No additional information is available."
  var level: Int = 0

  def process(tokenArray: Array[String]): Boolean


  def addSubCommand(cmd: Command) : Unit= {
    subCommandTree.put(cmd.commandToken, cmd)

  }

  def commandMatch(token:String):Boolean = {
    if(commandToken.startsWith(token)) true else false
  }

  def help(): String = {
    commandHelpText
  }

  def explain(): String = {
    val sb = new mutable.StringBuilder
    sb.append(commandToken)
    sb.append("\t\t")
    sb.append(commandDescription)
    sb.append("\n")

    subCommandTree.foreach((command) => {
      explain(command._2,sb)
    })

    sb.toString()
  }

  private def explain(cmd: Command, sb: mutable.StringBuilder): Unit = {

    cmd.level to 0 by -1 foreach(x => sb.append("\t"))
    sb.append(cmd.commandToken)
    sb.append("\t\t")
    sb.append(cmd.commandDescription)
    sb.append("\n")

    cmd.subCommandTree.foreach((command) => {
      explain(command._2,sb)
    })

  }
}
