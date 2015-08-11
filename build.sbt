import AssemblyKeys._

scalaVersion := "2.10.3"

assemblySettings

scalacOptions ++= Seq("-unchecked","-deprecation")

libraryDependencies += "edu.mit" % "jwi" % "2.2.1"

libraryDependencies += "org.twitter4j" % "twitter4j-core" % "[4.0,)"
