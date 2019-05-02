import sbtcrossproject.CrossPlugin.autoImport.crossProject

val sharedSettings = Seq(
  version in ThisBuild := "0.2",
  scalaVersion := "2.12.6",
  organization in ThisBuild := "ai.dragonfly.code",
  publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" )) ),
  scalacOptions in ThisBuild ++= Seq("-feature"),
  mainClass in (Compile, run) := Some("ai.dragonfly.math.vector.VectorTests")
)

val vector = crossProject(JSPlatform, JVMPlatform)
  .settings(sharedSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-dom_sjs0.6" % "0.9.7",
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  )