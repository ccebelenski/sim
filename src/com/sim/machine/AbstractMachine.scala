package com.sim.machine

import java.util.ServiceLoader

import com.sim.Named
import com.sim.device.BasicDevice

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

abstract class AbstractMachine extends Named{

  val description:String = "None"

  var devices: ListBuffer[BasicDevice] = new ListBuffer[BasicDevice]


}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = ServiceLoader.load(classOf[AbstractMachine]).asScala

}