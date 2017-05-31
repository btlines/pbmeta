lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion in ThisBuild := "2.12.2",
  version := "0.0.1",
  organization := "beyondthelines",
  licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil,
  bintrayOrganization := Some("beyondthelines"),
  bintrayPackageLabels := Seq("scala", "protobuf"),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val macros = project.settings(
  metaMacroSettings,
  name := "pbmeta",
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.7.0"
)

lazy val app = project.settings(
  metaMacroSettings,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
).dependsOn(macros)