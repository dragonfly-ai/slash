
import laika.helium.config.HeliumIcon
import laika.helium.config.IconLink
import laika.helium.Helium
import laika.format.Markdown
import laika.config.SyntaxHighlighting

val globalScalaVersion = "3.3.6"
ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion
ThisBuild / scalacOptions := Seq("semantic-db")
//ThisBuild / authors := List( "Someone" )

ThisBuild / tlFatalWarnings := false

ThisBuild / tlSitePublishBranch := Some("main")

ThisBuild / tlBaseVersion := "0.3" // this isn't used yet, until MIMA gets enabled by changing tlVersionIntroduced
ThisBuild / tlVersionIntroduced := Map("3" -> "1.0.0")
ThisBuild / tlCiReleaseBranches := Seq()

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
    libraryDependencies += "ai.dragonfly" %%% "narr" % "1.0.1"
  )
  .jvmSettings(
    libraryDependencies ++= Seq( "org.scala-js" %% "scalajs-stubs" % "1.1.0" )
  )
  .jsSettings()
  .nativeSettings()

lazy val verification = project // should move verification into JVM only tests.
  .dependsOn( slash.projects( JVMPlatform ) )
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "verification",
    Compile / mainClass := Some("verification.Verify"),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.6.1",
      "gov.nist.math" % "jama" % "1.0.3"
    )
  )

lazy val root = tlCrossRootProject.aggregate(slash, tests).settings(name := "slash")

lazy val jsdocs = project
  .in(file("jsdocs"))
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "io.github.quafadas" %%% "dedav_laminar" % "0.9.2",
  )
  .dependsOn(slash.js)
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(NoPublishPlugin)

lazy val docs = project
  .in(file("site"))
  .dependsOn(slash.jvm)
  .settings(
    mdocJS := Some(jsdocs),
    laikaExtensions := Seq(Markdown.GitHubFlavor, SyntaxHighlighting),
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
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test
  )