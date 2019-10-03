name := "mcmc"
version := "0.5.1"
organization := "org.compevol"
scalaVersion := "2.12.10"
crossScalaVersions := Seq("2.11.11", scalaVersion.value)
scalacOptions := { scalaVersion.value match {
  case "2.11.11" => Seq("-optimize")
  case _ => Seq("-opt:l:classpath")
}}
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "1.6.0"
libraryDependencies += "org.typelevel" %% "spire" % "0.17.0-M1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
