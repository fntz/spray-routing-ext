import sbt._
import sbt.Keys._
import xerial.sbt.Sonatype._
import xerial.sbt.Sonatype.SonatypeKeys._

object SprayRoutingExtBuild extends Build {

  val setting = Defaults.defaultSettings ++ Seq(
    scalacOptions ++= Seq("-Xlog-free-terms", "-deprecation", "-feature"),
    scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits"),
    resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
  )

  val sprayVersion = "1.3.1"

  lazy val mainProject = Project(
    id = "spray-routing-ext",
    base = file("."),
    settings = Project.defaultSettings ++ setting ++ sonatypeSettings ++ Seq(
      organization := "com.github.fntzr",
      name := "spray-routing-ext",
      version := "0.3.1",
      scalaVersion := "2.11.1",
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
        "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4",
        "org.scala-lang"    %  "scala-reflect"   % "2.11.1",
        "io.spray"          %%  "spray-can"     % sprayVersion,
        "io.spray"          %%  "spray-routing" % sprayVersion,
        "org.scalatest"     %% "scalatest" % "2.2.0" % "test",
        "org.scalactic"     %% "scalactic" % "2.2.0" % "test",
        "io.spray"          %%  "spray-testkit" % sprayVersion % "test"
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
       scalaVersion := "2.11.1",
       resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
       libraryDependencies ++= Seq(

       )
     )
  ) dependsOn(mainProject)

  val pomXml =
      <url>https://github.com/fntzr/spray-routing-ext</url>
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