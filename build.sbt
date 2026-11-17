ThisBuild / scalaVersion := "2.13.16"

name := "float-management-fsm"
version := "0.1.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.6.21",
  "org.scalameta" %% "munit" % "1.0.2" % Test
)
