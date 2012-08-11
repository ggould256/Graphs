import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

name := "graphs"

version := "0.1"

scalaVersion := "2.9.2"

resolvers += "twitter-repo" at "http://maven.twttr.com"

libraryDependencies ++= Seq(
  "com.twitter" % "finagle-core" % "1.9.0",
  "com.twitter" % "finagle-http" % "1.9.0",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "junit" % "junit" % "4.10"
)
