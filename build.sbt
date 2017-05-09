
organization := "com.github.workingDog"

name := "scalaxal"

version := (version in ThisBuild).value

scalaVersion := "2.11.11"

crossScalaVersions := Seq("2.11.11", "2.12.2")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.2",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6")

homepage := Some(url("https://github.com/workingDog/scalaxal"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

