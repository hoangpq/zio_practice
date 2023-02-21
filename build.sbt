ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "zio_practice",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.5",
      "dev.zio" %% "zio-test" % "2.0.5" % Test,
      "org.typelevel" %% "cats-core" % "2.1.0",
      "org.typelevel" %% "cats-effect" % "3.4.6"
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-deprecation"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
