name := "pbmeta"

version := "0.0.0"

scalaVersion in ThisBuild := "2.12.2"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val macros = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.7.0"
)

lazy val app = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
).dependsOn(macros)

