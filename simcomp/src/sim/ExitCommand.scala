package sim

import org.beryx.textio.TextIoFactory

class ExitCommand  extends Command {

  commandToken = "EXIT"
  commandDescription = "Exit the simulator."
  commandHelpText = "Exit the simulator, ending the program."

  override def process(tokenArray: Array[String]): Boolean = {


    //val bir = TextIoFactory.getTextIO.newBooleanInputReader()
    //bir.read("SIM: EXIT - Are you sure?")

    true
  }
}
