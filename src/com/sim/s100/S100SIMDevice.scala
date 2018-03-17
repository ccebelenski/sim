package com.sim.s100

import com.sim.Utils
import com.sim.cpu.Z80MMU
import com.sim.device.PortMappedDevice
import com.sim.unsigned.{UByte, UInt}

/**
  * A pseudo device for communication between the S100 and the simulator
  * Sits on Port 0xfe
  */
class S100SIMDevice(machine:S100Machine, mmu: Z80MMU, ports: List[UInt]) extends PortMappedDevice(machine,mmu,ports) {
  override def action(action: UInt, value: UByte, isWrite: Boolean): UByte = {

    action.toInt match {
      case 0xfe => {
        if(isWrite) simh_out(value)
        else simh_in(value)
      }
      case _ => UByte(0)
    }
  }

  override val description: String = "SIM Device"
  override val name = "SIM"

  override def init(): Unit = {}

  override def createUnitOptions: Unit = {}

  override def optionChanged(sb: StringBuilder): Unit = ???


  private def simh_out(byte:UByte) : UByte = {
    Utils.outln(s"Write 0xfe - $byte")
    byte.toInt match {

        case(14) => {
          //   Reset the SIM Pseudo Device
          // Nothing to do
        }

      case _ => {}
    }
    UByte(0x00)
  }

  private def simh_in(byte:UByte) : UByte = {
    Utils.outln(s"Read 0xfe - $byte")

    UByte(0x00)
  }
}
