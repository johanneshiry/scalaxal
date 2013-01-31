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
import xml.{Null, Attribute, NodeSeq, Text}
import scala.None


/**
 * @author Ringo Wathelet
 * Date: 01/02/13
 * Version: 1
 */

trait XalToXml[A] {
  def toXml(value: A): NodeSeq
}

trait XalToXmlSeq[A] {
  def toXml(value: A): Seq[NodeSeq]
}

/** Factory to convert xal objects instances to scala xml NodeSeq */
object XalToXml extends XmlExtractor {

  // ------------------------------------------------------------    
  // -----------------------implicit-----------------------------  
  // ------------------------------------------------------------  

  implicit def StringToXmlText(valueOption: Option[String]): Option[xml.Text] = {
    valueOption match {
      case Some(value) => Some(Text(value.trim))
      case None => None
    }
  }

  implicit object AddressDetailsToXml extends XalToXml[Option[AddressDetails]] {
    def toXml(addressDetailsOption: Option[AddressDetails]): NodeSeq = {
      addressDetailsOption match {
        case Some(addressDetails) => <AddressDetails
        AddressType={if (addressDetails.addressType.isDefined) addressDetails.addressType.get else null}
        CurrentStatus={if (addressDetails.currentStatus.isDefined) addressDetails.currentStatus.get else null}
        ValidFromDate={if (addressDetails.validFromDate.isDefined) addressDetails.validFromDate.get else null}
        ValidToDate={if (addressDetails.validToDate.isDefined) addressDetails.validToDate.get else null}
        Usage={if (addressDetails.usage.isDefined) addressDetails.usage.get else null}
        Code={if (addressDetails.code.isDefined) addressDetails.code.get else null}
        AddressDetailsKey={if (addressDetails.addressDetailsKey.isDefined) addressDetails.addressDetailsKey.get else null}>
          {"someting here"}
        </AddressDetails>
        case None => NodeSeq.Empty
      }
    }
  }

  implicit object AddressDetailsSeqToXml extends XalToXmlSeq[Option[Seq[AddressDetails]]] {
    def toXml(addressDetailsSet: Option[Seq[AddressDetails]]): Seq[NodeSeq] = {
      addressDetailsSet match {
        case Some(iSet) => (iSet collect {
          case x => getXmlFrom(Option(x.asInstanceOf[AddressDetails]))
        } filter (x => (x != null) && (x != None)) toSeq)
        case None => Seq.empty
      }
    }
  }

  implicit object XalToXml extends XalToXml[Option[XAL]] {
    def toXml(xalOption: Option[XAL]): NodeSeq = {
      xalOption match {
        case Some(xal) => <xAL xmlns:xal="urn:oasis:names:tc:ciq:xsdschema:xAL:2.0"
          Version={if (xal.version.isDefined) xal.version.get else null}>
          {getXmlSeqFrom(Option(xal.addressDetails))}
        </xAL>
        case None => NodeSeq.Empty
      }
    }
  }

  // ------------------------------------------------------------
  // -----------------------def----------------------------------  
  // ------------------------------------------------------------  

  /** this is the crux of getting xml from the xal objects */
  def getXmlFrom[A: XalToXml](xal: A) = implicitly[XalToXml[A]].toXml(xal)

  def getXmlSeqFrom[A: XalToXmlSeq](xal: A) = implicitly[XalToXmlSeq[A]].toXml(xal)

  def getNodeFromFieldName(name: String, objOption: Option[Any]): NodeSeq = {
    val baseName = if(name.startsWith("xal:")) name.substring(4) else name
    objOption match {
      case Some(obj) => {
        Some(obj.getClass.getDeclaredField(baseName)) match {
          case Some(field) => {
            field.setAccessible(true)
            val fieldValue = field.get(obj)
            if (fieldValue == null || !fieldValue.isInstanceOf[Option[_]]) NodeSeq.Empty
            else makeXmlNode(name, fieldValue.asInstanceOf[Option[_]])
          }
          case _ => NodeSeq.Empty
        }
      }
      case None => NodeSeq.Empty
    }
  }

  def makeXmlNode[_](name: String, value: Option[_]): NodeSeq = {
    if (value.isDefined) {
      value.get match {
        case bool: Boolean => <a> {if (bool) "1" else "0"} </a>.copy(label = name)
        case _ => <a> {value.get} </a>.copy(label = name)
      }
    } else NodeSeq.Empty
  }

}
