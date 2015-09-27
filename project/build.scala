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
    .settings (libraryDependencies += "org.spire-math" %% "cats" % "0.1.2")
    .settings (parallelExecution in ThisBuild := false)
}