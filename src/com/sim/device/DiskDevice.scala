package com.sim.device

import com.sim.machine.AbstractMachine

/**
  * Created by christophercebelenski on 7/18/16.
  */
abstract class DiskDevice(machine: AbstractMachine) extends BasicDevice(machine) {


  override def createUnitOptions: Unit = {

    unitOptions.append(EnumValueUnitOption("FORMAT", "Disk Format",
      List(UnitOptionValue("SIMH", true), UnitOptionValue("VHD", false))))
    unitOptions.append(BinaryUnitOption("READONLY","Read Only",false))
    unitOptions.append(BinaryUnitOption("DEBUG","Debug mode", false))
    unitOptions.append(BinaryUnitOption("REMOVABLE","Removable disk", false))


  }

}

