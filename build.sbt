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
  "-Xlint" ::
  "-Ypartial-unification" :: Nil

lazy val default = project
  .in (file ("."))
  .settings (moduleName := "hayago")
  .settings (organization := "io.github.sungiant")
  .settings (scalaVersion := "2.12.3")
  .settings (onLoadMessage := "")
  .settings (scalacOptions ++= compilerOptions)
  .settings (scalacOptions in (Compile, console) := compilerOptions)
  .settings (scalacOptions in (Compile, test) := compilerOptions)
  .settings (resolvers += "Sonatype" at "https://oss.sonatype.org/content/repositories/releases/")
  .settings (resolvers += "Typesafe" at "http://repo.typesafe.com/typesafe/releases/")
  .settings (libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF")
  .settings (libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.14.0")
  .settings (libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test")
  .settings (libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.0.2")
  .settings (parallelExecution in ThisBuild := false)
  .settings (connectInput in run := true)
  .settings (fork in run := true)
