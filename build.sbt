
version := "1.0.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"