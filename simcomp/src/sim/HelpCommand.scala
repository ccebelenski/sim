package sim

class HelpCommand extends Command {
  commandToken = "HELP"
  commandDescription = "Display command help."
  override def process(tokenArray: Array[String]) = {

    val sb : StringBuilder = new StringBuilder

    if(tokenArray.isEmpty) {
      // General help on all commands.

      sim.Console.commandTree.foreach((cmd) => {
        sb.append(cmd._2.explain())
      })

      sim.Console.textTerminal.println(sb.toString())
    } else {
      sim.Console.commandTree.find(_._2.commandMatch(tokenArray(0))).foreach(cmd => {sb.append(cmd._2.commandHelpText)})
      sim.Console.textTerminal.println(sb.toString())
    }
    false
  }
}