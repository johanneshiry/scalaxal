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

import xml._
import scala.Predef._
import com.scalaxal.xAL._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.{Option, Some}

/**
 * @author Ringo Wathelet
 *
 * converts xal objects to xml node sequences.
 *
 * Date: 01/02/13
 * Version: 1
 */

trait XalToXml[A] {
  def toXml(value: A): NodeSeq
}

/** Factory to convert XAL objects to scala xml NodeSeq */
object XalToXml extends XmlExtractor {

  def apply(obj: Any) = toXml(obj)

  implicit object AnyToXml extends XalToXml[Any] {
    def toXml(obj: Any): NodeSeq = {
      val objOption = if (obj.isInstanceOf[Option[_]]) obj else Option(obj)
      objOption match {
        case Some(x) => XalToXml(x)
        case None => NodeSeq.Empty
      }
    }
  }


  def getXmlFrom[A: XalToXml](xal: A) = implicitly[XalToXml[A]].toXml(xal)

//-----------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------

  /**
   * converts any xal object into a xml node sequence
   *
   * @param obj
   * @return
   */

  def toXml(obj: Any): NodeSeq = {
    val objOption = if (obj.isInstanceOf[Option[_]]) obj else Option(obj)
    objOption match {
      case Some(x) => (NodeSeq fromSeq fieldsToXml(obj))
      case None => NodeSeq.Empty
    }
  }

  private def capitalise(name: String) = if (!name.isEmpty) name(0).toUpper + name.substring(1) else name


  private def adjustLabel(name: String):String = {
    name match {
      case "objectType" | "ObjectType" => "Type"
      case "XAL" => "xAL"
      case _ => capitalise(name)
    }
  }

  // alternative to using annotations, just a basic lookup table, so much easier
  private def isAttribute(name: String):Boolean = {
    name match {
      case "ObjectType" |"Code" | "Type" | "TypeOccurrence" | "CurrentStatus" | "UsageType" |
      "AddressType" | "Usage" | "ValidFromDate" | "ValidToDate" | "AddressDetailsKey" |
      "PremiseDependencyType" | "PremiseDependency" | "Connector" |
      "DependentThoroughfares" | "DependentThoroughfaresIndicator" |
      "DependentThoroughfaresConnector" | "DependentThoroughfaresType" | "IndicatorOccurrence" |
      "Indicator" | "NameNumberOccurrence" | "NumberOccurrence" | "NumberType" |
      "NumberPrefixSeparator" | "NumberSuffixSeparator" | "Version" |
      "IdentifierType" | "Scheme" | "NumberTypeOccurrence" | "NumberExtensionSeparator" |
      "PremiseThoroughfareConnector" | "TypeOccurrence" | "PremiseNumberSeparator" |
      "RangeType" | "Separator" | "NumberRangeOccurrence" | "NameNumberSeparator"  => true
      case _ => false
    }
  }

  private def fieldsToXml(obj: Any) = {
     // the obj fields nodes to xml
     val fieldsXml = obj.getClass.getDeclaredFields.flatMap { field => {
         field.setAccessible(true)
         toXml(adjustLabel(field.getName), field.get(obj)) }
     }
     // create the NodeSeq with the class name, then add any other child fields nodes
     new Elem(null, adjustLabel(obj.getClass.getSimpleName), getAttributesOf(obj), TopScope, true, fieldsXml: _*)
   }

  private def toXml(name: String, value: Any): NodeSeq = {
    value match {
      case Some(x) => doMatch(name, x)
      case None => NodeSeq.Empty
      case x => doMatch(name, x)
    }
  }

  private def doMatch(name: String, value: Any) = {
    // special case, the "content" field name gives an Atom node
    if (name.equals("Content")) new Atom(value.toString)
    else {
      // do not process the attributes here
      if (isAttribute(capitalise(name))) NodeSeq.Empty
      else
        value match {
          case x: Content => getContentElem(name, x)
          case x: Seq[_] => x flatMap { v => toXml(name, v) }
          case x: String => new Elem(null, adjustLabel(name), Null, TopScope, true, Text(x.toString))
          case x: Int => new Elem(null, adjustLabel(name), Null, TopScope, true, Text(x.toString))
          case x: Boolean => new Elem(null, adjustLabel(name), Null, TopScope, true, Text(x.toString))
          case x: Double => new Elem(null, adjustLabel(name), Null, TopScope, true, Text(x.toString))
          case x: Any => fieldsToXml(x)
          case _ => NodeSeq.Empty
        }
    }
  }

  private def getAttributesOf(obj: Any): MetaData = {

    def getAttributesOf(obj: Any, ndx: Int): MetaData = {
      ndx match {
        case n if n <= 0 => Null
        case n if n >= 1 => {
          val ndxLessOne = n - 1
          val field = obj.getClass.getDeclaredFields.array(ndxLessOne)
          field.setAccessible(true)
          if (!isAttribute(capitalise(field.getName))) getAttributesOf(obj, ndxLessOne) else
            field.get(obj) match {
              case Some(x) => Attribute(None, adjustLabel(field.getName), Text(x.toString), getAttributesOf(obj, ndxLessOne))
              case _ => getAttributesOf(obj, ndxLessOne)
            }
        }
      }
    }

    getAttributesOf(obj, obj.getClass.getDeclaredFields.length)
  }

  // deals with the Content class objects
  private def getContentElem(name: String, obj: Any): Elem = {
    val field = obj.getClass.getDeclaredField("content")
    field.setAccessible(true)
    field.get(obj) match {
      case Some(x) => new Elem(null, adjustLabel(name), getAttributesOf(obj), TopScope, true, Text(x.toString))
      case _ => new Elem(null, adjustLabel(name), Null, TopScope, true, Text(""))
    }
  }

}
