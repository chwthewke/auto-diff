import sbt.Def
import sbt._
import sbt.Keys._

object Dependencies {
  type D = Seq[ModuleID]

  def group( organization: String, version: String )( artifacts: String* )( testArtifacts: String* ): D =
    artifacts.map( organization       %% _ % version ) ++
      testArtifacts.map( organization %% _ % version % "test" )

  def group( artifacts: ModuleID* )( testArtifacts: ModuleID* ): D =
    artifacts ++ (testArtifacts map (_ % "test"))

  val kindProjector: D = Seq( compilerPlugin( "org.spire-math" %% "kind-projector" % "0.9.4" ) )

  val splain: D = Seq( compilerPlugin( "io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch ) )

  val catsVersion = "0.9.0"

  val cats: D = group( "org.typelevel", catsVersion )( "cats-core" )( "cats" ) ++
    (group( "com.github.benhutchison" %% "mouse" % "0.9" )() map (_.exclude( "org.typelevel", "cats_2.12" ) ))

  val monocleVersion = "1.4.0"

  val monocle: D =
    group( "com.github.julien-truffaut", monocleVersion )( "monocle-core", "monocle-macro" )()

  val shapeless: D = group( "com.chuusai" %% "shapeless" % "2.3.2" )()

  val scalatestM: ModuleID = "org.scalatest" %% "scalatest" % "3.0.4"

  val scalacheckM: ModuleID = "org.scalacheck" %% "scalacheck" % "1.13.5"

  val scalacheck: D =
    group()( scalacheckM, "io.github.amrhassan" %% "scalacheck-cats" % "0.3.3" )

  val scalatest: D = group( scalatestM )()

  val scalatestForTests: D = group()( scalatestM )

  val enumeratumVersion: String = "1.5.12"
  val enumeratum: D             = group( "com.beachape" %% "enumeratum" % enumeratumVersion )()

  val overrides: Def.Setting[D] = dependencyOverrides ++= Seq(
    "org.scala-lang" % "scala-library"  % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
    scalacheckM
  )

  val common: D = kindProjector ++ cats ++ shapeless

  val settings: Seq[Def.Setting[_]] =
    Seq( libraryDependencies ++= common, overrides )
}
