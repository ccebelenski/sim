package com.sim.device

import com.sim.mux.{MuxDevice, MuxUnit}

trait MuxUnitAware {

  var attachedMuxDevice: Option[MuxDevice] = None
  var attachedMuxUnit: Option[MuxUnit] = None

  // This will be called when a character is available - the processing is of course
  // device specific.
  def muxCharacterInterrupt(char:Int) : Unit


  def writeChar(char:Int) : Unit = {
    if(attachedMuxUnit.isDefined) attachedMuxUnit.get.writeChar(char)
  }
}
