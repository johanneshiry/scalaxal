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
 * Date: 01/02/13
 * Version: 1
 */

/**
 * the extraction/creation of a xAL root element object from an xml node sequence
 * Specifically getting a xAL root object from <xAL> ... </xAL> nodeSeq
 */
trait XalExtractor {
  def makeXAL(nodeSeq: NodeSeq): Option[XAL]
}

/**
 * Reads a xAL root element (XAL) from various file, string and NodeSeq input sources
 *
 * @param xalExtractor the xALExtractor object used to extract xAL from xml, default xALFromXml
 * @param parser the SAX XML parser, default scala.xml.XML.parser
 * @see xALFromXml
 */

class XalFileReader(xalExtractor: Option[XalExtractor] = Some(XalFromXml),
                    parser: scala.xml.SAXParser = scala.xml.XML.parser) {

  /**
   * get a xAL root element from the input source
   *
   * @param source xAL input source, such as a file, a file name, a file descriptor
   * @return a xAL root element option
   */
  def loadXal(source: InputSource): Option[XAL] = {
    Some(loadXML(source, parser)) match {
      case Some(nodeSeq) => getXal(nodeSeq)
      case _ => None
    }
  }

  /**
   * get a sequence of Xal root element options from the input kmz file
   * @param file the input kmz file
   * @return a sequence of Xal root element options
   */
  def getXalFromZipFile(file: File): Seq[Option[XAL]] = {
    import scala.collection.JavaConversions._
    if (!file.getName.toLowerCase.endsWith(".zip")) Seq.empty
    else {
      (new java.util.zip.ZipFile(file).entries.
        filter(_.getName.toLowerCase.endsWith(".xml")).
        collect { case xmlFile => getXalFromFile(xmlFile.getName) } toSeq)
    }
  }

  /**
   * get a Xal root element from the input file
   * @param file the input xml file
   * @return a Xal root element option
   */
  def getXalFromFile(file: File): Option[XAL] = loadXal(fromFile(file))

  /**
   * get a Xal root element from the input file descriptor
   * @param fd the input xml file descriptor
   * @return a Xal root element option
   */
  def getXalFromFile(fd: FileDescriptor): Option[XAL] = loadXal(fromFile(fd))

  /**
   * get a Xal root element from the input file name
   * @param name the input file name
   * @return a Xal root element option
   */
  def getXalFromFile(name: String): Option[XAL] = loadXal(fromFile(name))

  /**
   * get a Xal root element from its input string representation
   * @param xmlString the input xml string
   * @return a Xal root element option
   */
  def getXalFromString(xmlString: String): Option[XAL] = getXal(XML.loadString(xmlString))

  /**
   * get a Xal root element from its kml NodeSeq
   * @param nodeSeq the input xml node sequence
   * @return a Xal root element option
   */
  def getXalFromNodeSeq(nodeSeq: scala.xml.NodeSeq): Option[XAL] = getXal(nodeSeq)

  /**
   * creates a Xal root element from the Node Sequence
   * @param nodeSeq the xml node sequence
   * @return a Xal root element option
   */
  private def getXal(nodeSeq: scala.xml.NodeSeq): Option[XAL] = {
    xalExtractor match {
      case Some(extractor) => extractor.makeXAL(nodeSeq)
      case _ => None
    }
  }
}

