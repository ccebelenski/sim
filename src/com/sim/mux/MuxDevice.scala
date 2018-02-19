package com.sim.mux

import java.io.IOException
import java.net.{ServerSocket, Socket}
import java.util.concurrent.{ExecutorService, Executors}

import com.sim.Utils
import com.sim.device.{BasicDevice, ValueUnitOption}
import com.sim.machine.AbstractMachine

/**
  * Psuedo device - System MUX - multiterminal telnet server.  Units are created dynamically as connections are made.
  * By convention the first connection is the system console or primary terminal - MUXA0.  Drivers written
  * to use the MUX, such as serial devices, should be aware that units are ephemeral and come and go as
  * connections are made and broken, and should respond appropriately.
  *
  * Multiple MUX devices may exist, depending on what is needed, but must run on different ports.
  *
  * Unlike other devices a simulator may use, the MUX isn't exposed directly to the simulator, but rather
  * through other simulated devices, such as terminal devices or serial ports.
  *
  * Unlike SIMH, serial I/O isn't supported at the moment.  This could be added here, or possibly with a
  * "serial mux" type of device.
  *
  * @param machine
  */
class MuxDevice(machine: AbstractMachine) extends BasicDevice(machine: AbstractMachine) {
  override val description: String = "System MUX - telnet handler"
  override val name = "MUX"
  var initComplete: Boolean = false
  var listenThread: Thread = _
  @volatile
  var socket: ServerSocket = _
  @volatile
  var executor: ExecutorService = _

  var clientCount:Int = 0

  var portnum:Int = 8888
  var maxClients:Int = 1

  var listener: MUXListener = new MUXListener(portnum, maxClients,this)
  /**
    * When init() completes, it will leave behind a thread for accepting connections on the port.
    * This thread will run for life of the simulator, and shouldn't need to be reset or recreated.
    */
  override def init(): Unit = {
    Utils.outln(s"$getName: Starting MUX.  Listening on port []")
    listenThread = new Thread(listener)
    listenThread.start()
    // TODO Probably never ends, so no join.
    initComplete = true
  }

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)


  }

  override def createUnitOptions: Unit = {
    unitOptions.append(new ValueUnitOption("PORTNUM", "Set port to listen on.", value = 8888))
    unitOptions.append(new ValueUnitOption("MAXCLIENTS", "Maximum # of clients that can connect", value = 1))
    unitOptions.append(new ValueUnitOption("TIMEOUT", "Network timeout value.", value = 8000))

  }
}


class MUXListener(val port: Int, val maxClients: Int, val device: MuxDevice) extends Runnable {

  def run(): Unit = {

    try {
      device.executor = Executors.newFixedThreadPool(maxClients)
      device.socket = new ServerSocket(port)
      while (true) {
        val s: Socket = device.socket.accept()
        if(device.clientCount < maxClients) {
          device.clientCount+=1
          val muxUnit = new MuxUnit(device, s)
          device.addUnit(muxUnit)
          device.executor.execute(muxUnit)
        } else {
          Utils.outln(s"\n\n${device.getName}: Max clients exceeded.")
          s.close()
        }
      }

    } catch {
      case t:Throwable =>Utils.outln(s"${device.getName}: Shutting down MUX server... ")
    } finally {
      device.executor.shutdown()
    }
  }
}

