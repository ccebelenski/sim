package com.sim.machine

import java.util.ServiceLoader

import com.sim.Named

import scala.collection.JavaConverters._

abstract class AbstractMachine extends Named{

  val description:String = "None"

}

object AbstractMachine {
  val services:Iterable[AbstractMachine] = ServiceLoader.load(classOf[AbstractMachine]).asScala

}