package sim

class VersionCommand extends Command {
  commandToken = "VERSION"
  commandDescription = "Display simulator version."

  override def process(tokenArray: Array[String]) = {

    Utils.outln("Sim 0.2 - C. Cebelenski 2017,2020")

    false
  }

}
