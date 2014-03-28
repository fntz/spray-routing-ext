import sbt._
import sbt.Keys._

object SprayRoutingExtBuild extends Build {

  lazy val mainProject = Project(
    id = "spray-routing-ext",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "spray-routing-ext",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.3",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.2.0",
        "org.scala-lang"    %  "scala-reflect"   % "2.10.3",
        "org.scalamacros"   %  "quasiquotes_2.10.3" % "2.0.0-M3",
        "io.spray"          %  "spray-can"     % "1.2.0",
        "io.spray"          %  "spray-routing" % "1.2.0",
        "org.scalatest"     %% "scalatest" % "2.0" % "test",
        "io.spray"          %  "spray-testkit" % "1.2.0" % "test"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
    )
  )

}