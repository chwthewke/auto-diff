import sbt.Def
import sbt._
import sbt.Keys._

object Dependencies {
  type D = Seq[ModuleID]

  val kindProjector: D = Seq(
    compilerPlugin( "org.typelevel" %% "kind-projector" % "0.10.3" cross CrossVersion.binary )
  )

  val splain: D = Seq( compilerPlugin( "io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch ) )

  val catsVersion = "2.0.0-RC2"

  val cats: D = Seq( "org.typelevel" %% "cats-core" % catsVersion )

  val shapeless: D = Seq( "com.chuusai" %% "shapeless" % "2.3.3" )

  val scalacheck: D =
    Seq(
      "org.scalacheck"    %% "scalacheck"      % "1.14.0"   % Test,
      "io.chrisdavenport" %% "cats-scalacheck" % "0.2.0-M1" % Test exclude ("org.typelevel", "cats-core")
    )

  val scalatest: D = Seq( "org.scalatest" %% "scalatest" % "3.0.8" )

  val scalatestForTests: D = scalatest.map( _ % Test )

  val enumeratum: D = Seq( "com.beachape" %% "enumeratum" % "1.5.13" )

  val overrides: Def.Setting[D] = dependencyOverrides ++= Seq(
    "org.scala-lang" % "scala-library"  % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
    "org.typelevel"  %% "cats-core"     % catsVersion
  ) ++ scalacheck

  val common: D = kindProjector ++ cats

  val settings: Seq[Def.Setting[_]] =
    Seq( libraryDependencies ++= common, overrides )
}
