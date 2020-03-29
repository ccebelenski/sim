package com.sim.term.actors

sealed trait CharacterReceived
final case class CharReceived(char: Char) extends CharacterReceived

object TerminalStream {

}