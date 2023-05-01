ThisBuild / version := "0.1.0-SNAPSHOT"

val scala3Version = "3.1.3"

val scalatest = Seq("org.scalatest" %% "scalatest" % "3.2.12" % Test)

lazy val root = (project in file("."))
  .settings(
    name := "syntax-analyser",
    scalaVersion := scala3Version,
    libraryDependencies ++= scalatest
  )
