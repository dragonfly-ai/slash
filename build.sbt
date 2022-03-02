ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val vector = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  name := "vector",
  version := "0.45",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  scalacOptions ++= Seq("-feature","-deprecation"),
  Compile / mainClass := Some("ai.dragonfly.math.example.Demo")
).jvmSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %% "scalajs-stubs" % "1.1.0"
  )
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "2.0.0"
  ),
  scalaJSUseMainModuleInitializer := true
)

lazy val verification = project.dependsOn(vector.projects(JVMPlatform)).settings(
  name := "verification",
  version := "0.01",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  scalacOptions ++= Seq("-feature","-deprecation"),
  Compile / mainClass := Some("ai.dragonfly.math.Verify"),
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1"
  )
)