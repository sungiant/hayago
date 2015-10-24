import sbt.Keys._
import sbt._

object SbtBuild extends Build {
  lazy val compilerOptions =
    "-deprecation" ::
    "-encoding" :: "UTF-8" ::
    "-feature" ::
    "-unchecked" ::
    "-language:_" ::
    "-Yno-adapted-args" ::
    "-Yrangepos" ::
    "-Ywarn-dead-code" ::
    "-Ywarn-numeric-widen" ::
    "-Xfuture" ::
    "-Xlint" :: Nil

  lazy val default = project
    .in (file ("."))
    .settings (moduleName := "hayago")
    .settings (organization := "io.github.sungiant")
    .settings (scalaVersion := "2.11.7")
    .settings (scalacOptions ++= compilerOptions)
    .settings (scalacOptions in (Compile, console) := compilerOptions)
    .settings (scalacOptions in (Compile, test) := compilerOptions)
    .settings (resolvers += "Sonatype" at "https://oss.sonatype.org/content/repositories/releases/")
    .settings (resolvers += "Typesafe" at "http://repo.typesafe.com/typesafe/releases/")
    .settings (libraryDependencies += "org.spire-math" %% "cats" % "0.2.0")
    .settings (libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.2.0")
    .settings (libraryDependencies += "org.specs2" %% "specs2-core" % "2.4.15" % "test")
    .settings (parallelExecution in ThisBuild := false)
    .settings (connectInput in run := true)
    .settings (fork in run := true)
}