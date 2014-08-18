import com.typesafe.sbt.SbtStartScript

import SbtStartScript.StartScriptKeys._

organization := "nxt"

name := "nxtscala"

version := "0.0.15"

scalaVersion := "2.10.4"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/"

libraryDependencies ++= Seq(
    "commons-lang" % "commons-lang" % "2.6" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "junit" % "junit" % "4.11" % "test"
)

publishMavenStyle := true

publishTo := Some(Resolver.file("nxtscala", new File( "../repo" )))