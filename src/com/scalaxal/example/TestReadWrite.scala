package com.scalaxal.example

import com.scalaxal.io.{XalToXml, XalPrintWriter, XalFileReader}
import xml.PrettyPrinter
import com.scalaxal.xAL.{AddressDetails, XAL}

/**
 * Author: Ringo Wathelet
 * Date: 4/02/13 
 * Version: 1
 */

object TestReadWrite {
  def main(args: Array[String]) {
    println("....XAL TestReadWrite start...\n")

    // create a xal object from an xml file
    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")

    if (xal.isDefined) {
      //          "./xal-files/scalaxal-XAL.XML"
      new XalPrintWriter().write(xal, new PrettyPrinter(80, 3))

      // write the AddressDetails
//      xal.get.addressDetails.foreach(x =>
//        XalToXml.toXml(x).foreach(z => println(new PrettyPrinter(80, 3).format(z))))

      // write the AddressDetails
//    xal.get.addressDetails.foreach(x => new XalPrintWriter().write(Option(x), new PrettyPrinter(80, 3)))
    }

    println("\n....XAL TestReadWrite done...")
  }

}
