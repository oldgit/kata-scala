name := """kata-scala"""

version := "0.1.0"

scalaVersion := "2.11.6"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

// Specs2
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.4" % "test",
  "org.specs2" %% "specs2-html" % "3.4" % "test")
