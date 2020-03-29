package sim.device

import sim.cpu.BasicMMU
import sim.machine.AbstractMachine
import sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class PortMappedDiskDevice(machine: AbstractMachine, mmu: BasicMMU, ports: List[UInt]) extends PortMappedDevice(machine, mmu, ports) {


  override def createUnitOptions: Unit = {

    unitOptions.append(EnumValueUnitOption("FORMAT", "Disk Format",
      List(UnitOptionValue("SIMH", true), UnitOptionValue("VHD", false))))
    unitOptions.append(BinaryUnitOption("READONLY","Read Only",false))
    unitOptions.append(BinaryUnitOption("DEBUG","Debug mode", false))
    unitOptions.append(BinaryUnitOption("REMOVABLE","Removable disk", false))


  }

}

