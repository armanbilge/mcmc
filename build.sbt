name := "mcmc"
version := "0.2"
organization := "org.compevol"
scalaVersion := "2.12.2"
crossScalaVersions := Seq("2.11.11", scalaVersion.value)
scalacOptions := { scalaVersion.value match {
  case "2.11.11" => Seq("-optimize")
  case _ => Seq("-opt:l:classpath")
}}
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "1.4.0"
libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
