lazy val root = project
  .in(file("."))
  .settings(

    scalaVersion := "3.3.1",

    libraryDependencies ++= Seq("com.github.sbt" % "junit-interface" % "0.13.3" % "test",
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"))
  )

  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
