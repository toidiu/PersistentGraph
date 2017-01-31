import sbt.Tests.{Group, SubProcess}

lazy val root = (project in file(".")).
  settings(
    name := "PersistGraph",
    version := "1.0",
    scalaVersion := "2.12.1"
  )


libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.6" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

//fork in test := true
fork in Test := true


// Define a method to group tests, in my case a single test per group
def singleTests(tests: Seq[TestDefinition]) =
  tests map { test =>
    Group(
      name = test.name,
      tests = Seq(test),
      runPolicy = SubProcess(javaOptions = Seq.empty[String]))
  }

// Add the following to the `Project` settings
testGrouping in Test <<= definedTests in Test map singleTests
