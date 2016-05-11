sbtPlugin := true

organization := "com.github.workingDog"

name := "scalaxal"

version := (version in ThisBuild).value

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8")

libraryDependencies ++= Seq("org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5")

homepage := Some(url("https://github.com/workingDog/scalaxal"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

