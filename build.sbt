name := """kata-scala"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

testOptions in Test += Tests.Argument("-oM", "-h", "target/html")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.pegdown" % "pegdown" % "1.5.0" % "test")
