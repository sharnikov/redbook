name := "redbook"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.23"


libraryDependencies += "co.fs2" %% "fs2-core" % "1.1.0-M1"
libraryDependencies += "co.fs2" %% "fs2-io" % "1.1.0-M1"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.3.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"