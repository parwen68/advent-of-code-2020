val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2020",
    version := "1.0.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.3",
      "org.scalatest" %% "scalatest" % "3.2.3" % "test"
    )
  )
