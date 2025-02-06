val crossVer = "1.3.2"
val scalaJSVersion = "1.18.2"
val scalaNativeVersion = "0.5.6"

addDependencyTreePlugin

// Scala Native support
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % crossVer)
addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)

// Scala.js support
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % crossVer)

// continuous integration
addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.7.7")

// Make me a website!
addSbtPlugin("org.typelevel" % "sbt-typelevel-site" % "0.7.7")