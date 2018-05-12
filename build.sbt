
organization := "com.github.workingDog"

name := "scalaxal"

version := (version in ThisBuild).value

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.12.6")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.6",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0")

homepage := Some(url("https://github.com/workingDog/scalaxal"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
