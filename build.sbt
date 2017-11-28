import sbt._
import sbt.Keys._

// format: off
scalaOrganization in ThisBuild := "org.scala-lang"
scalaVersion      in ThisBuild := "2.12.4"
conflictManager   in ThisBuild := ConflictManager.strict
// format: on

val sharedSettings = Seq( organization := "fr.thomasdufour" ) ++ Scalafmt.settings

val autodiffSettings =
  Defaults.coreDefaultSettings ++
    sharedSettings ++
    Scalac.settings ++
    Dependencies.settings :+
    (testOptions in Test += Tests.Argument( TestFrameworks.ScalaTest, "-oDF" ))

val `auto-diff-core` = project
  .settings( autodiffSettings )
  .settings( SbtBuildInfo.buildSettings( "AutodiffBuildInfo" ) )
  .settings( Console.coreImports.settings )

val `auto-diff-enumeratum` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.enumeratum )
  .dependsOn( `auto-diff-core` )

val `auto-diff-scalatest` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.scalatest )
  .dependsOn( `auto-diff-core` )

val `auto-diff-tests` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.scalatestForTests ++ Dependencies.scalacheck )
  .dependsOn( `auto-diff-core`, `auto-diff-enumeratum`, `auto-diff-scalatest` )

val `auto-diff` = project
  .in( file( "." ) )
  .settings( sharedSettings )
  .settings( publish := {}, publishLocal := {} )
  .aggregate( `auto-diff-core`, `auto-diff-enumeratum`, `auto-diff-scalatest`, `auto-diff-tests` )
