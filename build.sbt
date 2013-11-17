name := "yajom"

version := "0.1"

scalaVersion := "2.10.2"

organization := "org.gark87"

unmanagedSourceDirectories in Compile <+= baseDirectory(_ / "yajom-macros/src")

unmanagedSourceDirectories in Test <+= baseDirectory(_ / "yajom-example/src")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.scala-lang" % "scala-compiler" % "2.10.2"
)

testOptions in Test += Tests.Argument("-oDF")
