name := "bot"

version := "0.1"

scalaVersion := "2.13.0"

val sttpVersion = "1.6.4"
val circeVersion = "0.12.0-RC2"

libraryDependencies ++= Seq(
  "com.github.pureconfig" %% "pureconfig" % "0.11.1"
)

libraryDependencies ++= Seq(
  "com.softwaremill.sttp" %% "core",
  "com.softwaremill.sttp" %% "circe"
).map(_ % sttpVersion)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

