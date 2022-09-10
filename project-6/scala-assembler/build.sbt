ThisBuild / version := "0.1.0-SNAPSHOT"

val scala3Version = "3.1.3"

val scalatest = Seq("org.scalatest" %% "scalatest" % "3.2.12" % Test)
val catsEffect = Seq("org.typelevel" %% "cats-effect" % "3.3.13")

lazy val root = (project in file("."))
  .settings(
    name := "scala-assembler",
    scalaVersion := scala3Version,
    libraryDependencies ++= scalatest ++ catsEffect
  )
