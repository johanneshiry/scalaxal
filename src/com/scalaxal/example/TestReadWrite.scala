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

    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL-test.XML")

    if (xal.isDefined) {
      //              "./xal-files/scalaxal-XAL-test.XML"
      new XalPrintWriter("./xal-files/scalaxal-XAL-test.XML").write(xal, new PrettyPrinter(80, 3))
    }

    println("\n....XAL TestReadWrite done...")
  }
}
