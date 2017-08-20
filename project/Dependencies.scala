import sbt._
import sbt.Keys._

object Dependencies {
  type D = Seq[ModuleID]

  def group( organization: String, version: String )( artifacts: String* )( testArtifacts: String* ): D =
    artifacts.map( organization       %% _ % version withSources () ) ++
      testArtifacts.map( organization %% _ % version % "test" withSources () )

  def group( artifacts: ModuleID* )( testArtifacts: ModuleID* ): D =
    (artifacts map (_ withSources ())) ++ (testArtifacts map (_ % "test" withSources ()))

  val kindProjector: D = Seq( compilerPlugin( "org.spire-math" %% "kind-projector" % "0.9.4" ) )

  val catsVersion = "0.9.0"

  val cats: D = group( "org.typelevel", catsVersion )( "cats-core" )( "cats" ) ++
    group( "com.github.benhutchison" %% "mouse" % "0.9" )() map (_.exclude( "org.typelevel", "cats_2.12" ) )

  val scalatest: D = group()( "org.scalatest" %% "scalatest" % "3.0.3" )

  val scalacheck: D =
    group()( "org.scalacheck" %% "scalacheck" % "1.13.4", "io.github.amrhassan" %% "scalacheck-cats" % "0.3.3" )

  val monocleVersion = "1.4.0"

  val monocle: D =
    group( "com.github.julien-truffaut", monocleVersion )( "monocle-core", "monocle-macro" )()

  val shapeless: D = group( "com.chuusai" %% "shapeless" % "2.3.2" )()

  val common: D = kindProjector ++ cats ++ shapeless ++ scalacheck ++ scalatest
}
