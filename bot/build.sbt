name := "bot"

version := "0.1"

scalaVersion := "2.12.0"

val sttpVersion = "1.6.4"
val pureconfigVersion = "0.11.1"
val circeVersion = "0.12.0-RC2"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0-RC1"

libraryDependencies ++= Seq(
  "com.github.pureconfig" %% "pureconfig",
  "com.github.pureconfig" %% "pureconfig-cats-effect"
).map(_ % pureconfigVersion)

libraryDependencies ++= Seq(
  "com.softwaremill.sttp" %% "core",
  "com.softwaremill.sttp" %% "circe",
  "com.softwaremill.sttp" %% "async-http-client-backend-cats"
).map(_ % sttpVersion)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

