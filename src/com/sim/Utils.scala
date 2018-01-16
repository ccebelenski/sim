package com.sim

import com.sim.unsigned.UInt

import scala.collection.immutable.HashMap

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
 /* def getUint(cptr: String, radix: Int, max: Int) : Int = {
    val value: UInt = strtotv(cptr,radix)

    if(value > max) throw new IllegalArgumentException("Maximum value exceeded")

    value
  }
*/
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
  def strtotv(inptr: String, radix: Int): Int = {
    var value: Int = 0
    var digit: Int = 0

    if ((radix < 2) || (radix > 36)) return value
    val trimmed = inptr.trim.toUpperCase.toCharArray


    trimmed.foreach(c => {
      if (!isalnum(c)) return value
      if (c.isDigit) digit = c - '0'
      else if (radix <= 10) return value
      else digit = c + 10 - 'A'
      if (digit >= radix) return value
      value = (value * radix) + digit
    })
    value

  }

  import java.util.regex.Pattern

  def toBytes(filesize: String): Long = {
    var returnValue : Long = -1
    val patt = Pattern.compile("([\\d.]+)([GMK]B)", Pattern.CASE_INSENSITIVE)
    val matcher = patt.matcher(filesize)
    val powerMap : Map[String,Int] = HashMap[String,Int]("GB" ->3, "MB"->2, "KB"-> 1)
    if (matcher.find) {
      val number = matcher.group(1)

      val pow :Int = powerMap(matcher.group(2).toUpperCase)
      var bytes = BigDecimal(number)
      bytes = bytes * BigDecimal(1024).pow(pow)
      returnValue = bytes.toLong
    }
    returnValue
  }


  def outln(msg:String) : Unit = {
    if(Console.textTerminal != null) Console.textTerminal.println(msg)
    else System.out.println(msg)
  }
}
