package com.scalaxal.example

import com.scalaxal.io.{XalToXml, XalPrintWriter, XalFileReader}
import xml.PrettyPrinter

/**
 * Author: Ringo Wathelet
 * Date: 4/02/13 
 * Version: 1
 */

object TestReadWrite {
  def main(args: Array[String]) {
    println("....XAL TestReadWrite start...\n")

    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")

    if (xal.isDefined) {
      //              "./xal-files/scalaxal-XAL.XML"
      new XalPrintWriter().write(xal, new PrettyPrinter(80, 3))
    }

    println("\n....XAL TestReadWrite done...")
  }
}
