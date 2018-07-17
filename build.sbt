name := "fp-in-scala"

version := "1.0"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

