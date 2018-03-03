package com.sim.s100

import com.sim.cpu.{BasicMMU, Z80MMU}
import com.sim.device.{BasicDevice, MuxUnitAware, PortMappedUnit}
import com.sim.unsigned.UInt

/**
  * A simulated MITS 2SIO interface card.
  *
  * The card had two physical I/O ports which could be connected
  * to any serial I/O device that would connect to a current loop,
  * RS232, or TTY interface. Available baud rates were jumper
  * selectable for each port from 110 to 9600.
  * All I/O is via programmed I/O. Each device has a status port
  * and a data port. A write to the status port can select
  * some options for the device (0x03 will reset the port).
  * A read of the status port gets the port status:
  * +---+---+---+---+---+---+---+---+
  * | X | X | X | X | X | X | O | I |
  * +---+---+---+---+---+---+---+---+
  * I - A 1 in this bit position means a character has been received
  * on the data port and is ready to be read.
  * O - A 1 in this bit means the port is ready to receive a character
  * on the data port and transmit it out over the serial line.
  * A read to the data port gets the buffered character, a write
  * to the data port writes the character to the device.
  *
  * @param device
  * @param mmu
  * @param ports List of ports this unit responds to
  */
class S100SIOUnit(device: S100SIODevice, mmu: Z80MMU, ports: List[UInt]) extends PortMappedUnit(device: BasicDevice, mmu: BasicMMU, ports: List[UInt])
  with MuxUnitAware {
  override def init(): Unit = {} // TODO

  // Supports file attaching
  override val supportsAttach = true

  override def cancel(): Unit = ???

  override def completeAction(): Unit = ???

  override def showCommand(sb: StringBuilder): Unit = {
    super.showCommand(sb)
  }

  override def optionChanged(sb: StringBuilder): Unit = ???

  override def muxCharacterInterrupt(char: Int): Unit = ???
}
