import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Test",
    libraryDependencies += scalaTest % Test
  )
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
