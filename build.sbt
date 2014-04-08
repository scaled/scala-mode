crossPaths := false

scalaVersion := "2.11.0-RC1"

autoScalaLibrary := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize",
                      "-language:postfixOps" /*, "-Yinline-warnings"*/)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

libraryDependencies ++= Seq(
  "com.samskivert.scaled" % "scaled" % "1.0-SNAPSHOT",
  "com.samskivert.scaled" % "textmate-grammar" % "1.0-SNAPSHOT",
  "com.novocode" % "junit-interface" % "0.10" % "test"
)
