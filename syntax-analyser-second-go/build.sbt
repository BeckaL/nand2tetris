ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

val scalatest = Seq("org.scalatest" %% "scalatest" % "3.2.12" % Test)
val scalaXml = Seq("org.scala-lang.modules" %% "scala-xml" % "2.3.0")

lazy val root = (project in file("."))
  .settings(
    name := "syntax-analyser",
    libraryDependencies ++= scalatest ++ scalaXml
  )