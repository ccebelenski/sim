package com.sim

trait Named {

  private var name: String = _

  def setName(name:String) : Unit = {this.name = name}
  def getName() : String = name
}
