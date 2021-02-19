ThisBuild / scalaVersion := "2.13.3"

lazy val root = project.in(file(".")).aggregate(vector.js, vector.jvm)

lazy val vector = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  name := "vector",
  version := "0.201",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai:4343/",
  scalacOptions ++= Seq("-feature","-deprecation"),
  mainClass in (Compile, run) := Some("ai.dragonfly.math.vector.VectorTests")
).jvmSettings(
  libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0"
).jsSettings(
  libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "1.1.0" exclude("org.scala-js", "scalajs-library")),
  scalaJSUseMainModuleInitializer := true
)