name := "advent"

lazy val `day-01` = project

lazy val `day-02` = project
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.2.2",
      "co.fs2" %% "fs2-io" % "3.2.2",
      "org.typelevel" %% "kittens" % "2.3.2",
    )
  )