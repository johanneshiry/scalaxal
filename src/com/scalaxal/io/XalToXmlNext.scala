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
import scala.reflect.runtime.{universe => ru}
import com.scalaxal.xAL._
import java.lang.annotation.{RetentionPolicy, Retention, Annotation}
import scala.Some
import com.scalaxal.xAL.BuildingName
import com.scalaxal.xAL.AttributeField
import java.lang.reflect.Method
import scala.Some
import com.scalaxal.xAL.BuildingName

/**
 * @author Ringo Wathelet
 * Date: 01/02/13
 * Version: 1
 */

import com.scalaxal.xAL.Types._
case class TestAClass(@Retention(RetentionPolicy.RUNTIME)
                      @Attrib
                      version: String)

/** Factory to convert XAL objects to scala xml NodeSeq */
object XalToXmlNext extends XmlExtractor {

  def getXmlFrom[A: XalToXml](xal: A) = XalToXmlNext.toXml(xal)

  // todo TypeOccurrence, attributes, DependentThoroughfares, RangeType, NumberType, NumberOccurrence

  def main(args: Array[String]) {
    println("....XalToXmlNext start...\n")
    val xal = new XalFileReader().getXalFromFile("./xal-files/XAL.XML")
    XalToXmlNext.toXml(xal.get).foreach(x => println(new PrettyPrinter(80, 3).format(x)))

//    val xal = new TestAClass(version = "23")
//    annotationTest(xal)

    println("\n....XalToXmlNext done...")
  }

  def annotationTest(xal: TestAClass) {
    import com.scalaxal.xAL.Types._

    val testClassSymbol = ru.typeOf[xal.type].typeSymbol.asClass

    println("testClassSymbol="+testClassSymbol)

    val testAnnotations = testClassSymbol.annotations
    println("testAnnotations="+testAnnotations)

    val testAnnotationType = ru.typeOf[Attrib]

    println("vvvvv==="+ru.typeOf[xal.type].typeSymbol.annotations)

    println("testAnnotationType="+testAnnotationType+" test="+
      testAnnotations.find(a => a.tpe == testAnnotationType).isDefined)

//   println("ttttt="+getMeAnnotation(xal, classOf[AttributeField]))

    val fieldList = xal.getClass.getDeclaredFields.foreach(field => {
      field.setAccessible(true)
      println("field name = " + field.getName + "  field value = " + field.get(xal))

      field.getAnnotations.foreach(x => println("x="+x))

 //     println(" anno = "+field.getAnnotation[AttributeField](classOf[AttributeField]))

 //     println(" anno = "+field.isAnnotationPresent(testAnnotationType) )

    })

         // .getAnnotations.foreach(x => println("x="+x))
//    val field = xal.getClass.getField("version")
//    field.setAccessible(true)
//    println("field.getAnnotations="+field.getAnnotations.isEmpty)

 //   val anno = field.getAnnotation[AttributeField](classOf[AttributeField])
 //   println("anno="+anno.toString)

//    for (anno <- field.getAnnotations) println(" anno = "+anno )

 //   field.getAnnotations.foreach(x => println(" anno = "+x ))

  }

  def getMeAnnotation[A <: Annotation](obj:Object, annotationCls:Class[A]) : Option[Annotation] = {
    Option (obj.getClass.getAnnotation (annotationCls))
  }

  def capitalise(name: String) = {
    if (!name.isEmpty) name(0).toUpper + name.substring(1) else name
  }

  def deCapitalise(name: String) = {
    if (!name.isEmpty) name(0).toLower + name.substring(1) else name
  }

  def lookupLabel(name: String):String = {
    name match {
      case "objectType" | "ObjectType" => "Type"
      case _ => capitalise(name)
    }
  }

  // DependentThoroughfares is both an attribute and a class
  def isAttribute(name: String):Boolean = {
    name match {
      case "Code" | "ObjectType" | "TypeOccurrence" | "CurrentStatus" |
      "AddressType" | "Usage" | "ValidFromDate" | "ValidToDate" |
      "PremiseDependencyType" | "PremiseDependency" | "Connector" |
      "DependentThoroughfares" | "DependentThoroughfaresIndicator" |
      "DependentThoroughfaresConnector" | "IndicatorOccurrence" |
      "Indicator" | "NameNumberOccurrence" => true
      case _ => false
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
     new Elem(null, lookupLabel(obj.getClass.getSimpleName), getAttributesOf(obj), TopScope, true, fieldsXml: _*)
   }

 def doMatch(name: String, value: Any) = {
   value match {
     case x: BuildingName => withAttributesToXml(name, x)
     case x: ContentType => withAttributesToXml(name, x)
     case x: Seq[_] => x flatMap {v => toXml(name, v)}
     case x: String => new Elem(null, lookupLabel(name), Null, TopScope, true, Text(x.toString))
     case x: Int => new Elem(null, lookupLabel(name), Null, TopScope, true, Text(x.toString))
     case x: Boolean => new Elem(null, lookupLabel(name), Null, TopScope, true, Text(x.toString))
     case x: Double => new Elem(null, lookupLabel(name), Null, TopScope, true, Text(x.toString))
     case x: Any => fieldsToXml(x)
     case _ => NodeSeq.Empty
   }
 }

  def toXml(name: String, value: Any): NodeSeq = {
    value match {
      case Some(x) => doMatch(name, x)
      case None => NodeSeq.Empty
      case _ => doMatch(name, value)
    }
  }

  def getAttributesOf(obj: Any): MetaData = {
    def getAttributesOf(obj: Any, ndx: Int): MetaData = {
      ndx match {
        case 0 => Null
        case n if n >= 1 => {
          val ndxLessOne = n - 1
          val field = obj.getClass.getDeclaredFields.array(ndxLessOne)
          field.setAccessible(true)
          val attribs = getAttributesOf(obj, ndxLessOne)
          if (!isAttribute(capitalise(field.getName)) || (field.getName.equals("content"))) attribs else
            field.get(obj) match {
              case Some(x) => Attribute(None, lookupLabel(field.getName), Text(x.toString), attribs)
              case _ => attribs
            }
        }
      }
    }
    getAttributesOf(obj, obj.getClass.getDeclaredFields.length)
  }

  def withAttributesToXml(name: String, obj: Any): Elem = {
    val field = obj.getClass.getDeclaredField("content")
    field.setAccessible(true)
    field.get(obj) match {
      case Some(x) => new Elem(null, lookupLabel(name), getAttributesOf(obj), TopScope, true, Text(x.toString))
      case _ => new Elem(null, lookupLabel(name), Null, TopScope, true, Text(""))
    }
  }

//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------

}
