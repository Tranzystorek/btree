lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "pl.edu.pw.ii",
      scalaVersion := "2.12.5",
      version	   := "0.1.0-SNAPSHOT"
    )),
    name := "btree"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
