package com.sim.device

import java.nio.file.Path

trait UnitAttachable {

  def attach(sectorSize: Int, xferElementSize: Int, autosize: Boolean, driveType: String, pdp11Tracksize: Int, completetionDelay: Int) : StringBuilder

  def detach()

  // Attached path
  var attachedPath: Option[Path] = None
  // Does this unit support being attached?
  val supportsAttach: Boolean = true

  def showAttachedInfo(sb: StringBuilder): Unit = {

    if (supportsAttach && attachedPath.isDefined) sb.append(s"Attached: ${attachedPath.get.getFileName.toString}\n")
  }

}
