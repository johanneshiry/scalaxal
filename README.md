# **scalaxal** an extensible Address Language (xAL) library written in scala.


## Overview

The extensible Address Language (xAL) is a unified hierarchical address format developed
by the OASIS technical committee using xml. This **scalaxal** library represents addresses,
as specified by the standard, as scala objects. It also provides for reading addresses
in xml format into **scalaxal** objects, and writing **scalaxal** objects to xml format.

## References
 
1) The Organization for the Advancement of Structured Information Standards [OASIS](https://www.oasis-open.org/committees/ciq/)

2) Extensible Address Language (xAL) Standard Description Document for W3C DTD/Schema Version 2.0

## Packages

The **scalaxal** library is in 2 major parts:

1) package com.scalaxal.xAL, the set of xAL classes

2) package com.scalaxal.io, the reading and writing of xAL from/to xml

## Documentation

See the OASIS xAL Standard v2.0
 
1) https://www.oasis-open.org/committees/ciq/ciq.html

2) https://www.oasis-open.org/committees/ciq/download.html

Also the OASIS xAL Standard v2.0 in pdf is included here in the doc directory.
  
## Installation

Add the following dependency to build.sbt:

    libraryDependencies += "com.github.workingDog" %% "scalaxal" % "1.2"

To compile and generate a jar file from the source:

    sbt package

The jar file (scalaxal_2.12-1.3-SNAPSHOT.jar) will be in the "./target/scala-2.12" directory.

## Dependencies

**scalaxal** depends on the standard [Scala XML library](https://github.com/scala/scala-xml)  

## Usage

    object ReadWriteExample1 {
      def main(args: Array[String]) {
       val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")
       if (xal.isDefined) {
         val writer = new XalPrintWriter()
         writer.write(xal, new PrettyPrinter(80, 3))
         writer.close()
       }
     }
    }

## Status

Stable

Tested on the XAL.XML and XAL_AU.XML example files from OASIS, included here in the xal-files directory.

