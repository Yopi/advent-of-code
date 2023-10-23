ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    idePackagePrefix := Some("org.adventofcode")
  )

libraryDependencies += "org.json4s" %% "json4s-native" % "4.1.0-M2"
libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0"