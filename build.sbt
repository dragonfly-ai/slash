ThisBuild / scalaVersion := "3.2.1"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven" ) ) )
ThisBuild / resolvers += "ai.dragonfly.code" at "https://code.dragonfly.ai/"
ThisBuild / organization := "ai.dragonfly.code"
ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")

lazy val vector = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "vector",
    version := "0.5321",
    libraryDependencies += "ai.dragonfly.code" %%% "narr" % "0.0321",
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0"
    )
  ).jsSettings()

lazy val verification = project
  .dependsOn(vector.projects(JVMPlatform))
  .settings(
    name := "verification",
    version := "0.01",
    Compile / mainClass := Some("ai.dragonfly.math.Verify"),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.6.1"
    )
  )

lazy val demo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .dependsOn(vector)
  .settings(
    name := "demo",
    Compile / mainClass := Some("Demo"),
    libraryDependencies ++= Seq(
//      "com.lihaoyi" %%% "scalatags" % "0.11.1",
      "ai.dragonfly.code" %%% "democrossy" % "0.0105"
    ),
    Compile / mainClass := Some("Demo")
  ).jsSettings(
    Compile / fastOptJS / artifactPath := file("./demo/public_html/js/main.js"),
    scalaJSUseMainModuleInitializer := true
  ).jvmSettings()
