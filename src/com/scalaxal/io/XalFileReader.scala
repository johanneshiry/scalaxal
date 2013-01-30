/*
 * Copyright (c) 2013, Ringo Wathelet
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 * - Neither the name of "scalaxal" nor the names of its contributors may
 *   be used to endorse or promote products derived from this software without
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.scalaxal.io

import com.scalaxal.xAL._
import org.xml.sax.InputSource
import scala.xml.XML._
import scala.xml.{NodeSeq, XML}
import java.io.{ InputStream, Reader, StringReader, File, FileDescriptor, FileInputStream }
import scala.xml.Source._
import scala.language.postfixOps
/**
 * @author Ringo Wathelet
 * Date: 12/12/12
 * Version: 1
 */

/**
 * the extraction/creation of a kml root element object from an xml node sequence
 * Specifically getting a kml root object from <kml> ... </kml> nodeSeq
 */
trait XalExtractor {
  def makeXAL(nodeSeq: NodeSeq): Option[XAL]
}

/**
 * Reads a kml root element (Kml) from various file, string and NodeSeq input sources
 *
 * @param xalExtractor the KmlExtractor object used to extract kml from xml, default KmlFromXml
 * @param parser the SAX XML parser, default scala.xml.XML.parser
 * @see KmlFromXml
 */

class KmlFileReader(xalExtractor: Option[XalExtractor] = Some(XalFromXml),
                    parser: scala.xml.SAXParser = scala.xml.XML.parser) {

  /**
   * get a Kml root element from the input source
   *
   * @param source kml input source, such as a file, a file name, a file descriptor
   * @return a Kml root element option
   */
  def loadKml(source: InputSource): Option[XAL] = {
    Some(loadXML(source, parser)) match {
      case Some(nodeSeq) => getKml(nodeSeq)
      case _ => None
    }
  }

  /**
   * get a sequence of Kml root element options from the input kmz file
   * @param file the input kmz file
   * @return a sequence of Kml root element options
   */
  def getKmlFromKmzFile(file: File): Seq[Option[XAL]] = {
    import scala.collection.JavaConversions._
    if (!file.getName.toLowerCase.endsWith(".kmz")) Seq.empty
    else {
      (new java.util.zip.ZipFile(file).entries.
        filter(_.getName.toLowerCase.endsWith(".kml")).
        collect { case kmlFile => getKmlFromFile(kmlFile.getName) } toSeq)
    }
  }

  /**
   * get a Kml root element from the input file
   * @param file the input xml file
   * @return a Kml root element option
   */
  def getKmlFromFile(file: File): Option[XAL] = loadKml(fromFile(file))

  /**
   * get a Kml root element from the input file descriptor
   * @param fd the input xml file descriptor
   * @return a Kml root element option
   */
  def getKmlFromFile(fd: FileDescriptor): Option[XAL] = loadKml(fromFile(fd))

  /**
   * get a Kml root element from the input file name
   * @param name the input file name
   * @return a Kml root element option
   */
  def getKmlFromFile(name: String): Option[XAL] = loadKml(fromFile(name))

  /**
   * get a Kml root element from its input string representation
   * @param xmlString the input xml string
   * @return a Kml root element option
   */
  def getKmlFromString(xmlString: String): Option[XAL] = getKml(XML.loadString(xmlString))

  /**
   * get a Kml root element from its kml NodeSeq
   * @param nodeSeq the input xml node sequence
   * @return a Kml root element option
   */
  def getKmlFromNodeSeq(nodeSeq: scala.xml.NodeSeq): Option[XAL] = getKml(nodeSeq)

  /**
   * creates a Kml root element from the Node Sequence
   * @param nodeSeq the xml node sequence
   * @return a Kml root element option
   */
  private def getKml(nodeSeq: scala.xml.NodeSeq): Option[XAL] = {
    xalExtractor match {
      case Some(extractor) => extractor.makeXAL(nodeSeq)
      case _ => None
    }
  }
}

