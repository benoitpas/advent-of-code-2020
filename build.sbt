lazy val root = project
  .in(file("."))
  .settings(

    scalaVersion := "3.0.2",

    libraryDependencies ++= Seq("com.novocode" % "junit-interface" % "0.11" % "test",
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"))
  )

  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
