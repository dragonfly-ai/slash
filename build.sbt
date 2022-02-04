ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val vector = crossProject(JSPlatform, JVMPlatform).settings(
  publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
  name := "vector",
  version := "0.304",
  organization := "ai.dragonfly.code",
  resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
  scalacOptions ++= Seq("-feature","-deprecation"),
  Compile / mainClass := Some("ai.dragonfly.math.vector.Demo")
).jvmSettings(
  libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0"
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "2.0.0"
  ),
  //excludeDependencies += ExclusionRule(organization = "org.scala-js"),
  scalaJSUseMainModuleInitializer := true
)