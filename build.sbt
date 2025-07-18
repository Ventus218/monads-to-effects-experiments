val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "monads-to-effects-experiments",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "io.github.dotty-cps-async" %% "dotty-cps-async" % "1.1.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "io.getkyo" %% "kyo-prelude" % "0.19.0",
    libraryDependencies += "io.getkyo" %% "kyo-direct" % "0.19.0",
    libraryDependencies += "io.getkyo" %% "kyo-core" % "0.19.0"
  )
