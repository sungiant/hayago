import sbt.Keys._
import sbt._

object SbtBuild extends Build {
  lazy val default = project
    .in (file ("."))
    .settings (moduleName := "hayago")
    .settings (organization := "io.github.sungiant")
    .settings (scalaVersion := "2.11.7")
    .settings (resolvers += "Sonatype" at "https://oss.sonatype.org/content/repositories/releases/")
    .settings (resolvers += "Typesafe" at "http://repo.typesafe.com/typesafe/releases/")
    .settings (libraryDependencies += "org.spire-math" %% "cats" % "0.2.0")
    .settings (libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.2.0")
    .settings (parallelExecution in ThisBuild := false)
    .settings (connectInput in run := true)
    .settings (fork in run := true)
}