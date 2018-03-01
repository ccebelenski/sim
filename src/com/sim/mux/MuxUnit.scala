package com.sim.mux

import java.net.{Socket, SocketAddress}

import com.sim.Utils
import com.sim.device.{BasicDevice, BasicUnit, ValueUnitOption}
import com.sim.unsigned.{UByte, UInt}

class MuxUnit(device: BasicDevice, var socket: Socket) extends BasicUnit(device: BasicDevice) with Runnable {

  private val socketAddress: SocketAddress = socket.getRemoteSocketAddress
  private val outputStream = socket.getOutputStream
  private val inputStream = socket.getInputStream

  @volatile
  var char: Int = 0

    def getTimeout: Int = {
    val o = getValueOption("TIMEOUT")
    if (o.isDefined) o.get else 5000

  }



  override def init(): Unit = {
    socket.setSoTimeout(getTimeout)
    Utils.out(s"\n\n$getName: Telnet connection from: $socketAddress\n\n")
  }


  // Called when options changed - this is only really going to be useful on init, since
  // most options can't be changed after the socket is open.
  override def optionChanged(sb:StringBuilder) : Unit = {

    // PORT isn't a unit option
    // MAXCLIENTS isn't a unit option
    // Timeout can change, but not for the unit in progress.
  }

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)

    sb.append(s"Connected to: ${socketAddress}\n")
  }

  override def handles(value: UInt): Boolean = ???

  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = ???

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  def writeChar(char: Int): Unit = {
    if (socket.isConnected) {
      outputStream.write(char)
      outputStream.flush()
    }
  }

  override def run(): Unit = {
    while (!socket.isClosed && char != -1) {
      try {

        char = inputStream.read()

      } catch {
        case t: Throwable => {}
        case i:InterruptedException => {}
      }
      finally {
        if(socket.isClosed || char == -1) {
          socket.close()
          Utils.out(s"\n\n$getName: Telnet session terminated.\n\n")
          device.removeUnit(this)
          device.asInstanceOf[MuxDevice].clientCount-=1
          return
        }
      }
    }
  }
}
