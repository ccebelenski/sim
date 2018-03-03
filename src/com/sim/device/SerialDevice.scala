package com.sim.device

trait SerialDevice extends SupportsOptions{

  def createSerialUnitOptions: Unit = {
    unitOptions.append(BinaryUnitOption("ANSI", "Set bit 8 of output to 0", value = false))
    unitOptions.append(BinaryUnitOption("UPPER", "Convert input to upper case", value = false))
    unitOptions.append(BinaryUnitOption("BS", "Map delete to backspace", value = false))
  }


}
