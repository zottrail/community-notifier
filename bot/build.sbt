name := "bot"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "ujson" % "0.7.5",
  "com.softwaremill.sttp" %% "core" % "1.6.4",
  "com.github.pureconfig" %% "pureconfig" % "0.11.1"
)