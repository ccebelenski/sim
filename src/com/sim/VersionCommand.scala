package com.sim

class VersionCommand extends Command {
  commandToken = "VERSION"
  commandDescription = "Display simulator version."

  override def process(tokenArray: Array[String]) = {

    Console.textTerminal.println("Sim 0.1 - C. Cebelenski 2017")

    false
  }

}
