import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration
import com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration
import ReleaseTransformations._

// format: off
scalaOrganization in ThisBuild := "org.scala-lang"
scalaVersion      in ThisBuild := "2.12.4"
conflictManager   in ThisBuild := ConflictManager.strict
// format: on

val sharedSettings = Seq(
  organization := "fr.thomasdufour",
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  )
) ++
  Scalafmt.settings

val autodiffSettings =
  Defaults.coreDefaultSettings ++
    sharedSettings ++
    Scalac.settings ++
    Dependencies.settings :+
    (testOptions in Test += Tests.Argument( TestFrameworks.ScalaTest, "-oDF" ))

def withOverwrite( enable: Boolean )( config: PublishConfiguration ): PublishConfiguration =
  config.withOverwrite( enable )

val publishSettings = Seq(
  useGpg := true,
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const( false ),
  sonatypeProfileName := "thomas_dufour",
  publishMavenStyle := true,
  licenses := Seq( "APL2" -> url( "http://www.apache.org/licenses/LICENSE-2.0.txt" ) ),
  homepage := Some( url( "https://github.com/chwthewke/auto-diff" ) ),
  scmInfo := Some(
    ScmInfo( url( "https://github.com/chwthewke/auto-diff" ), "scm:git:git@github.com:chwthewke/auto-diff.git" ) ),
  developers := List(
    Developer( id = "chwthewke",
              name = "Thomas Dufour",
              email = "td@thomasdufour.fr",
              url = url( "https://github.com/chwthewke" ) )
  ),
  publishConfiguration := withOverwrite( isSnapshot.value )( publishConfiguration.value ),
  publishSignedConfiguration := withOverwrite( isSnapshot.value )( publishSignedConfiguration.value ),
  publishLocalConfiguration ~= withOverwrite( enable = true ),
  publishLocalSignedConfiguration ~= withOverwrite( enable = true )
)

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  setReleaseVersion,
  runClean,
  runTest,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand( "publishSigned" ),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand( "sonatypeReleaseAll" ),
  pushChanges
)

releaseVersionBump := sbtrelease.Version.Bump.Minor

val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

val `auto-diff-core` = project
  .settings( autodiffSettings )
  .settings( SbtBuildInfo.buildSettings( "AutodiffBuildInfo" ) )
  .settings( Console.coreImports.settings )
  .settings( publishSettings )

val `auto-diff-enumeratum` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.enumeratum )
  .settings( publishSettings )
  .dependsOn( `auto-diff-core` )

val `auto-diff-scalatest` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.scalatest )
  .settings( publishSettings )
  .dependsOn( `auto-diff-core` )

val `auto-diff-tests` = project
  .settings( autodiffSettings )
  .settings( Console.coreImports.settings )
  .settings( libraryDependencies ++= Dependencies.scalatestForTests ++ Dependencies.scalacheck ++ Dependencies.splain )
  .settings( noPublishSettings )
  .dependsOn( `auto-diff-core`, `auto-diff-enumeratum`, `auto-diff-scalatest` )

val `auto-diff` = project
  .in( file( "." ) )
  .settings( sharedSettings )
  .settings( Dependencies.overrides )
  .settings( noPublishSettings )
  .aggregate( `auto-diff-core`, `auto-diff-enumeratum`, `auto-diff-scalatest`, `auto-diff-tests` )
