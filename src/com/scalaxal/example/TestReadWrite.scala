package com.scalaxal.example

import com.scalaxal.io.{XalToXml, XalPrintWriter, XalFileReader}
import xml.PrettyPrinter
import com.scalaxal.xAL.{AddressLine, AddressLines, AddressDetails, XAL}

/**
 * Author: Ringo Wathelet
 * Date: 4/02/13 
 * Version: 1
 */

object TestReadWrite {
  def main(args: Array[String]) {

    test2
  }


  def test1() {
    println("....XAL TestReadWrite test1 start...\n")

    // create a xal object from an xml file
    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")

    if (xal.isDefined) {
      // create a new AddressDetails with an specific AddressLine
      val newAddressDetails = AddressDetails(addressDetailsType =
        Some(new AddressLines(addressLines = (Seq.empty :+ new AddressLine(Some("a new address line"))))))

      // create a new xal object with the new AddressDetails
      val newXal = xal.get.copy(addressDetails = (xal.get.addressDetails :+ newAddressDetails))

      //          "./xal-files/scalaxal-XAL.XML"
      new XalPrintWriter().write(Option(newXal), new PrettyPrinter(80, 3))

      // write the AddressDetails
//      xal.get.addressDetails.foreach(x =>
//        XalToXml.toXml(x).foreach(z => println(new PrettyPrinter(80, 3).format(z))))

      // write the AddressDetails
//    xal.get.addressDetails.foreach(x => new XalPrintWriter().write(Option(x), new PrettyPrinter(80, 3)))
    }

    println("\n....XAL TestReadWrite test1 done...")
  }

  def test2() {

    println("....XAL TestReadWrite test2 start...\n")

    // create a xal object from an xml file
    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")

    if (xal.isDefined) {

      // create a new AddressLines and add a new AddressLine
      val newAddressLines = AddressLines() addToAddressLines (new AddressLine(Some("a new address line")))

      val newAddressDetails = AddressDetails().copy(addressDetailsType = Some(newAddressLines))

      // create a new xal object with the new AddressDetails
      val newXal = xal.get addToAddressDetails (newAddressDetails)

      //          "./xal-files/scalaxal-XAL.XML"
      new XalPrintWriter().write(Option(newXal), new PrettyPrinter(80, 3))

      // write the AddressDetails
      //      xal.get.addressDetails.foreach(x =>
      //        XalToXml.toXml(x).foreach(z => println(new PrettyPrinter(80, 3).format(z))))

      // write the AddressDetails
   //   newXal.addressDetails.foreach(x => new XalPrintWriter().write(Option(x), new PrettyPrinter(80, 3)))
    }

    println("\n....XAL TestReadWrite test2 done...")

  }

}
