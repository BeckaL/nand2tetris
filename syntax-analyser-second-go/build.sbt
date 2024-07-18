ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

val scalatest = Seq("org.scalatest" %% "scalatest" % "3.2.12" % Test)

lazy val root = (project in file("."))
  .settings(
    name := "syntax-analyser",
    libraryDependencies ++= scalatest
  )