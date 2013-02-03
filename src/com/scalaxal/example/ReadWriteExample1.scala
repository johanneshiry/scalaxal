package com.scalaxal.example

import com.scalaxal.io.{XalPrintWriter, XalFileReader, XalFromXml}
import xml.PrettyPrinter

/**
 * Author: Ringo Wathelet
 * Date: 31/01/13 
 * Version: 1
 */

object ReadWriteExample1 {
  def main(args: Array[String]) {
    println("....XAL ReadWriteExample1 start...\n")

    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")
    if (xal.isDefined) new XalPrintWriter().write(xal, new PrettyPrinter(80, 3))

    println("\n....XAL ReadWriteExample1 done...")
  }
}
