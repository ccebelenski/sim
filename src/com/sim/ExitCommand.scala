package com.sim

import org.beryx.textio.{BooleanInputReader, TextIoFactory}

class ExitCommand  extends Command {

  commandToken = "EXIT"
  commandDescription = "Exit the simulator."
  commandHelpText = "Exit the simulator, ending the program."

  override def process(tokenArray: Array[String]): Boolean = {


    val bir = TextIoFactory.getTextIO.newBooleanInputReader()
    bir.read("Are you sure?")

  }
}
