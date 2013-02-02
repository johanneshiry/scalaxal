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
import scala.Some
import com.scalaxal.xAL.Content

/**
 * @author Ringo Wathelet
 * Date: 01/02/13
 * Version: 1
 */

case class TestInner(inner: Option[String] = None,
                     inType: Double,
                     inCode: Option[Boolean] = None)

case class TestAddress(outer: Option[String] = None,
                       outMap: Seq[String] = Nil,
                       outCode: Option[Boolean] = None,
                       addressLatitude: Option[Content],
                       outObject: Option[TestInner])

/** Factory to convert XAL object to scala xml NodeSeq */
object XalToXmlNext extends XmlExtractor {

  def getXmlFrom[A: XalToXml](xal: A) = XalToXmlNext.toXml(xal)

  // todo TypeOccurrence, attributes, DependentThoroughfares, RangeType, NumberType, NumberOccurrence


  def main(args: Array[String]) {
    println("....XalToXmlNext start...\n")

    val address = new TestAddress(outer = Some("zzouterthing"),
      outMap = Seq("zztestString1", "zztestString2"), outCode = Some(true),
      addressLatitude = Some(Content(content=Some("zzblabla"),
        objectType=Some("zzobjectType"), code=Some("zzcode"), attributes = Map())),
      outObject = Some(new TestInner(Some("zzinner"), inType= 5.6)))

    XalToXmlNext.toXml(address).foreach(x => println(new PrettyPrinter(80, 3).format(x)))

    println("\n....XalToXmlNext done...")
  }

  def capitalise(name: String) = {
    if (!name.isEmpty) name(0).toUpper + name.substring(1) else name
  }

  def lookupLabel(name: String):String = {
    name match {
      case "objectType" | "ObjectType" => "Type"
      case _ => name
    }
  }


//---------------------------------------------------------------------------------------------------
  def toXml(theObject: Any): NodeSeq = {
    NodeSeq fromSeq fieldsToXml(theObject)
  }

   def fieldsToXml(obj: Any) = {
     // the fields nodes
     val fieldsXml = obj.getClass.getDeclaredFields.flatMap { field => {
         field.setAccessible(true)
         toXml(lookupLabel(field.getName), field.get(obj))
       }
     }
     // start the NodeSeq with the class name, then add the fields nodes
     new Elem(null, lookupLabel(obj.getClass.getSimpleName), Null, TopScope, true, fieldsXml: _*)
   }

 def doMatch(name: String, value: Any)={
   value match {
     case x: Content => contentToXml(name, x)
     case x: Seq[_] => x flatMap {v => toXml(name, v)}
     case x: String => new Elem(null, capitalise(name), Null, TopScope, true, Text(x.toString))
     case x: Int => new Elem(null, capitalise(name), Null, TopScope, true, Text(x.toString))
     case x: Boolean => new Elem(null, capitalise(name), Null, TopScope, true, Text(x.toString))
     case x: Double => new Elem(null, capitalise(name), Null, TopScope, true, Text(x.toString))
     case x: Any => new Elem(null, capitalise(name), Null, TopScope, true, fieldsToXml(x): _*)
     case _ => NodeSeq.Empty
   }
 }

  def contentToXml(name: String, value: Content) = {
    val attributes = Attribute(None, "Code", Text(value.code.getOrElse("")),
      Attribute(None, "Type", Text(value.objectType.getOrElse("")), Null))
    new Elem(null, capitalise(name), attributes, TopScope, true, Text(value.content.getOrElse("")))
  }

  def toXml(name: String, value: Any): NodeSeq = {
//    println("name = " + name + " , value = " + value)
    value match {
      case Some(x) => doMatch(name, x)
      case None => NodeSeq.Empty
      case _ => doMatch(name, value)
    }
  }

//---------------------------------------------------------------------------------------------------

}
