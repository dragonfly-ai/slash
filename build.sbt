import laika.ast
import laika.ast.Path.Root
import laika.helium.config.HeliumIcon
import laika.helium.config.IconLink
import java.time.Instant
import laika.helium.Helium
import laika.helium.config.Favicon
import laika.helium.config.ImageLink
import laika.ast.*
import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting

val appVersion:String = "0.1"
val globalScalaVersion = "3.3.0"

ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion
//ThisBuild / authors := List( "Someone" )

ThisBuild / tlFatalWarnings := false

ThisBuild / tlSitePublishBranch := Some("main")

ThisBuild / tlBaseVersion := appVersion
ThisBuild / tlCiReleaseBranches := Seq()
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / nativeConfig ~= {
  _.withLTO(scala.scalanative.build.LTO.thin)
    .withMode(scala.scalanative.build.Mode.releaseFast)
    .withGC(scala.scalanative.build.GC.commix)
}

ThisBuild / tlVersionIntroduced := Map("3" -> "1.0.0")

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

lazy val jsdocs = project
  .in(file("jsdocs"))
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "io.github.quafadas" %%% "dedav_laminar" % "0.9-2b8610f-20230922T150429Z-SNAPSHOT",
  )
  .dependsOn(slash.js)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(NoPublishPlugin)

lazy val docs = project
.in(file("site"))
.dependsOn(slash.jvm)
.settings(
  mdocJS := Some(jsdocs),
  laikaExtensions := Seq(GitHubFlavor, SyntaxHighlighting),
  laikaConfig ~= { _.withRawContent },
  //tlSiteHeliumExtensions :=  Seq(GitHubFlavor, SyntaxHighlighting),
  tlSiteHelium := {
    Helium.defaults.site.metadata(
      title = Some("S"),
      language = Some("en"),
      description = Some("S"),
      authors = Seq("one"),
    )
    .site
    .topNavigationBar(
      homeLink = IconLink.internal(laika.ast.Path(List("index.md")), HeliumIcon.home),
      navLinks = Seq(IconLink.external("https://github.com/dragonfly-ai/slash", HeliumIcon.github))
    )
    .site
    .externalJS(
      url = "https://cdn.jsdelivr.net/npm/vega@5"
    )
    .site
    .externalJS(
      url = "https://cdn.jsdelivr.net/npm/vega-lite@5"
    )
    .site
    .externalJS(
      url = "https://cdn.jsdelivr.net/npm/vega-embed@6"
    )
    .site
    .autoLinkJS()
  }
)
.enablePlugins(TypelevelSitePlugin)
.enablePlugins(NoPublishPlugin)

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