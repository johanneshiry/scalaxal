package com.scalaxal.io


import xml._
import scala.Predef._
import scala.Some
import com.scalaxal.xAL.Content


case class TestInner(inner: Option[String] = None,
                     inType: Double,
                     inCode: Option[Boolean] = None)

case class TestAddress(outer: Option[String] = None,
                       outMap: Seq[String] = Nil,
                       outCode: Option[Boolean] = None,
                       addressLatitude: Option[Content],
                       outObject: Option[TestInner])


object XalToXmlNext {

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
