organization := "nxt"

name := "nxtscala"

version := "0.0.37"

scalaVersion := "2.10.4"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.10.4",
    "com.github.nscala-time" %% "nscala-time" % "0.4.2",
    "com.jsuereth" %% "scala-arm" % "1.3",
    "commons-lang" % "commons-lang" % "2.6" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "junit" % "junit" % "4.11" % "test"
)

publishMavenStyle := true

publishTo := Some(Resolver.file("nxtscala", new File( "../repo" )))
