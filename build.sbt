val appVersion:String = "0.101"
val globalScalaVersion = "3.3.0"

ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion

ThisBuild / tlBaseVersion := appVersion
ThisBuild / tlCiReleaseBranches := Seq()
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / nativeConfig ~= {
  _.withLTO(scala.scalanative.build.LTO.thin)
    .withMode(scala.scalanative.build.Mode.releaseFast)
    .withGC(scala.scalanative.build.GC.commix)
}

lazy val slash = crossProject(
    JSPlatform,
    JVMPlatform,
    NativePlatform
  )
  .crossType(CrossType.Full)
  .settings(
    description := "High performance, low footprint, cross platform, Linear Algebra and Statistics Hacks!",
    libraryDependencies += "ai.dragonfly" %%% "narr" % "0.101"
  )
  .jvmSettings(
    libraryDependencies ++= Seq( "org.scala-js" %% "scalajs-stubs" % "1.1.0" )
  )
  .jsSettings()
  .nativeSettings()

lazy val verification = project
  .dependsOn( slash.projects( JVMPlatform ) )
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "verification",
    Compile / mainClass := Some("ai.dragonfly.math.Verify"),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "gov.nist.math" % "jama" % "1.0.3"
    )
  )

lazy val demo = crossProject(
  JSPlatform,
  JVMPlatform,
  NativePlatform
)
  .crossType(CrossType.Full)
  .enablePlugins(NoPublishPlugin)
  .dependsOn(slash)
  .settings(
    name := "demo",
    Compile / mainClass := Some("Demo"),
    libraryDependencies ++= Seq(
      "ai.dragonfly" %%% "cliviz" % "0.102",
      "ai.dragonfly" %%% "democrossy" % "0.102"
    ),
    Compile / mainClass := Some("Demo")
  )
  .jsSettings(
    Compile / fullOptJS / artifactPath := file("./docs/js/main.js"),
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings()
  .nativeSettings()

lazy val root = tlCrossRootProject.aggregate(slash, tests).settings(name := "slash")

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin).settings(
  mdocVariables := Map(
    "VERSION" -> appVersion,
    "SCALA_VERSION" -> globalScalaVersion
  ),
  laikaConfig ~= { _.withRawContent }
)

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin) // also enables the ScalaUnidocPlugin
  .settings(
    name := "slash-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inProjects(
        slash.jvm,
        slash.js,
        slash.native
      )
  )

lazy val tests = crossProject(
    JVMPlatform,
    JSPlatform,
    NativePlatform
  )
  .in(file("tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(slash)
  .settings(
    name := "slash-tests",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M8" % Test
  )
  // .jvmSettings(name := "tests-jvm")
  // .jsSettings(name := "tests-js")
  // .nativeSettings(name := "tests-native")