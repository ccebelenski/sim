package com.sim.mux

import java.net.{Socket, SocketAddress}

import com.sim.Utils
import com.sim.device.{BasicDevice, BasicUnit}
import com.sim.unsigned.{UByte, UInt}

class MuxUnit(device: BasicDevice, var socket: Socket) extends BasicUnit(device: BasicDevice) with Runnable {

  private val socketAddress: SocketAddress = socket.getRemoteSocketAddress
  private val outputStream = socket.getOutputStream
  private val inputStream = socket.getInputStream

  @volatile
  var char: Int = 0

  var timeout: Int = 5000


  override def init(): Unit = {
    socket.setSoTimeout(timeout)
    Utils.out(s"\n\n$getName: Telnet connection from: ${socketAddress}\n\n")
  }

  override def showCommand(sb: StringBuilder): Unit = ???

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
