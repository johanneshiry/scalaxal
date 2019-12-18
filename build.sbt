
organization := "com.github.workingDog"

name := "scalaxal"

version := (version in ThisBuild).value

scalaVersion := "2.13.0"

crossScalaVersions := Seq("2.13.0")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.13.0",
  "org.scala-lang.modules" %% "scala-xml" % "2.0.0-M1")

homepage := Some(url("https://github.com/workingDog/scalaxal"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
