version := "1.0"

name := "Machine Learning"

scalaVersion := "2.12.1"

javaSource in Compile := baseDirectory.value / "src" / "main" / "java7"

unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "test" / "java7"

fork in Test := true

libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.1" % "test",
        "org.scalacheck" %% "scalacheck" % "latest.release" % Test,
        "com.google.guava" % "guava" % "20.0"
)
