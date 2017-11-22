import sbt._
import sbt.Keys._

// format: off
scalaOrganization in ThisBuild := "org.scala-lang"
scalaVersion      in ThisBuild := "2.12.4"
conflictManager   in ThisBuild := ConflictManager.strict
// format: on

val sharedSettings = Seq( organization := "net.chwthewke" ) ++ Scalafmt.settings

val autodiffSettings =
  Defaults.coreDefaultSettings ++
    sharedSettings ++
    Scalac.settings ++
    Dependencies.settings :+
    (testOptions in Test += Tests.Argument( TestFrameworks.ScalaTest, "-oDF" ))

val `auto-diff-core` = project
  .settings( autodiffSettings )
  .settings( SbtBuildInfo.buildSettings( "AutodiffCoreBuildInfo" ) )
  .settings( Console.coreImports.settings )

val `auto-diff` = project
  .in( file( "." ) )
  .settings( sharedSettings )
  .settings( publish := {}, publishLocal := {} )
  .aggregate( `auto-diff-core` )
