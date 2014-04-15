import sbt._
import sbt.Keys._

import aether._


object SprayRoutingExtBuild extends Build {

  val setting = Defaults.defaultSettings ++ Aether.aetherSettings ++ Seq(
    scalacOptions ++= Seq("-Xlog-free-terms", "-deprecation", "-feature"),
    scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")
  )


  lazy val mainProject = Project(
    id = "spray-routing-ext",
    base = file("."),
    settings = Project.defaultSettings ++ setting ++ Seq(
      organization := "com.github.fntzr",
      name := "spray-routing-ext",
      version := "0.1",
      scalaVersion := "2.10.3",
      licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
      publishTo <<= version { v: String =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      publishMavenStyle := true,
      pomExtra := pomXml,
      publishArtifact in Test := false,
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.3.0",
        "org.scala-lang"    %  "scala-reflect"   % "2.10.3",
        "org.scalamacros"   %  "quasiquotes_2.10.3" % "2.0.0-M3",
        "io.spray"          %  "spray-can"     % "1.3.1",
        "io.spray"          %  "spray-routing" % "1.3.1",
        "org.scalatest"     %% "scalatest" % "2.0" % "test",
        "io.spray"          %  "spray-testkit" % "1.3.1" % "test"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
    )
  )

  lazy val sampleProject = Project(
    id = "sample",
    base = file("sample"),
    settings = Project.defaultSettings ++ setting ++ Seq(
      name := "ext-sample",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.3",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies ++= Seq(

      )
    )
  ) dependsOn(mainProject)

  val pomXml =
      <url>https://github.com/fntzr/spray-routing-ext</url>
      <licenses>
        <license>
          <name>MIT</name>
          <url>http://opensource.org/licenses/MIT</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:fntzr/spray-routing-ext.git</url>
        <connection>scm:git:git@github.com:fntzr/spray-routing-ext.git</connection>
      </scm>
      <developers>
        <developer>
          <id>fntzr</id>
          <name>Mike</name>
          <url>https://github.com/fntzr</url>
        </developer>
      </developers>
}