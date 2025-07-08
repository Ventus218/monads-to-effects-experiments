val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "monads-to-effects-experiments",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
