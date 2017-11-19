package com.sim

import com.sim.unsigned.UInt

/**
  * Created by christophercebelenski on 7/19/16.
  */
object Utils {
  /* get_uint             unsigned number
     Inputs:
          cptr    =       pointer to input string
          radix   =       input radix
          max     =       maximum acceptable value
          *status =       pointer to error status
     Outputs:
          val     =       value
  */
  def getUint(cptr: String, radix: UInt, max: UInt) : UInt = {
    val value: UInt = strtotv(cptr,radix)

    if(value > max) throw new IllegalArgumentException("Maximum value exceeded")

    value
  }

  def isalnum(c: Char) : Boolean = {
    if ((c & 0x80) != 0) false else c.isLetterOrDigit
  }

  /** Radix independent input/output package
    * strtotv - general radix input routine
    * Inputs:
    * inptr   =       string to convert
    * radix   =       radix for input
    * Outputs:
    * value   =       converted value
    */
  def strtotv(inptr: String, radix: UInt): UInt = {
    var value: UInt = UInt(0)
    var digit: UInt = UInt(0)

    if ((radix < UInt(2)) || (radix > UInt(36))) return value
    val trimmed = inptr.trim.toUpperCase.toCharArray


    trimmed.foreach(c => {
      if (!isalnum(c)) return value
      if (c.isDigit) digit = UInt(c - '0')
      else if (radix <= UInt(10)) return value
      else digit = UInt(c + 10 - 'A')
      if (digit >= radix) return value
      value = (value * radix) + digit
    })
    value

  }
}
