import sbt._
import sbt.Keys._
import xerial.sbt.Sonatype._
import xerial.sbt.Sonatype.SonatypeKeys._

object SprayRoutingExtBuild extends Build {

  val setting = Defaults.defaultSettings ++ Seq(
    scalacOptions ++= Seq("-Xlog-free-terms", "-deprecation", "-feature"),
    scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")
  )

  val sprayVersion = "1.3.1"

  lazy val mainProject = Project(
    id = "spray-routing-ext",
    base = file("."),
    settings = Project.defaultSettings ++ setting ++ sonatypeSettings ++ Seq(
      organization := "com.github.fntzr",
      name := "spray-routing-ext",
      version := "0.2.3",
      scalaVersion := "2.10.4",
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
        "com.typesafe.akka" %% "akka-actor" % "2.3.6" % "provided",
        "org.scala-lang"    %  "scala-reflect"   % "2.10.4",
        "org.scalamacros"   %%  "quasiquotes" % "2.0.1",
        "io.spray"          %  "spray-can"     % sprayVersion % "provided",
        "io.spray"          %  "spray-routing" % sprayVersion % "provided",
        "org.scalatest"     %% "scalatest" % "2.0" % "test",
        "io.spray"          %  "spray-testkit" % sprayVersion % "test"
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )
  )

  lazy val sampleProject = Project(
    id = "sample",
    base = file("sample"),
    settings = Project.defaultSettings ++ setting ++ Seq(
      name := "ext-sample",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.4",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies ++= Seq(

      )
    )
  ) dependsOn(mainProject)

  val pomXml =
      <url>https://github.com/fntz/spray-routing-ext</url>
      <scm>
        <url>git@github.com:fntz/spray-routing-ext.git</url>
        <connection>scm:git:git@github.com:fntz/spray-routing-ext.git</connection>
      </scm>
      <developers>
        <developer>
          <id>fntz</id>
          <name>Mike</name>
          <url>https://github.com/fntz</url>
        </developer>
      </developers>
}