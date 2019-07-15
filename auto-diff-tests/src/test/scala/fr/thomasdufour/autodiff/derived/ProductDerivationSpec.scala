package fr.thomasdufour.autodiff
package derived

import cats.syntax.apply._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProductDerivationSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import DiffOps._
  import Nudge._
  import ProductDerivationSpec._

  "A derived Diff for a simple case class" should {

    val diff: Diff[Edge] = semi.diff

    "report differences between different values" when {
      "the first field is different" in {
        val diffEdges: Gen[( Edge, Edge )] = for {
          edge     <- genEdge
          diffFrom <- name.except( edge.from )
        } yield ( edge, edge.copy( from = diffFrom ) )

        forAll( diffEdges ) {
          case ( x, y ) =>
            diff( x, y ).tree should ===( F( "Edge", "from" -> (x.from !== y.from) ) )
        }
      }

      "the second field is different" in {
        val diffEdges: Gen[( Edge, Edge )] = for {
          edge   <- genEdge
          diffTo <- name.except( edge.to )
        } yield ( edge, edge.copy( to = diffTo ) )

        forAll( diffEdges ) {
          case ( x, y ) =>
            diff( x, y ).tree should ===( F( "Edge", "to" -> (x.to !== y.to) ) )
        }
      }

      "both fields are different" in {
        val diffEdges: Gen[( Edge, Edge )] = for {
          edge     <- genEdge
          diffFrom <- name.except( edge.from )
          diffTo   <- name.except( edge.to )
        } yield ( edge, Edge( diffFrom, diffTo ) )

        forAll( diffEdges ) {
          case ( x, y ) =>
            diff( x, y ).tree should ===( F( "Edge", "from" -> (x.from !== y.from), "to" -> (x.to !== y.to) ) )
        }
      }
    }

    "report no difference between identical values" in {
      forAll( genEdge ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "show a value" in {
      forAll( genEdge ) { x =>
        diff.show( x ) should ===( s"Edge(from: ${x.from}, to: ${x.to})" )

      }
    }
  }

  "A derived Diff for a nested case class" should {

    val diff: Diff[WeightedEdge] = semi.diff

    "report differences between different values" when {
      "the first field is different" in {
        val diffEdges: Gen[( WeightedEdge, WeightedEdge )] = for {
          edge       <- genWeightedEdge
          diffWeight <- arbitrary[Int].except( edge.weight )
        } yield ( edge, edge.copy( weight = diffWeight ) )

        forAll( diffEdges ) {
          case ( x, y ) =>
            diff( x, y ).tree should ===( F( "WeightedEdge", "weight" -> (x.weight.toString !== y.weight.toString) ) )
        }
      }

      "an inner field is different" in {
        val diffEdges: Gen[( WeightedEdge, WeightedEdge )] = for {
          edge     <- genWeightedEdge
          diffFrom <- name.except( edge.edge.from )
        } yield ( edge, edge.copy( edge = edge.edge.copy( from = diffFrom ) ) )

        forAll( diffEdges ) {
          case ( x, y ) =>
            diff( x, y ).tree should ===(
              F( "WeightedEdge", "edge" -> F( "Edge", "from" -> (x.edge.from !== y.edge.from) ) )
            )
        }
      }

    }

    "report no difference between identical values" in {
      forAll( genWeightedEdge ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "show a value" in {
      forAll( genWeightedEdge ) { x =>
        diff.show( x ) should ===(
          s"WeightedEdge(weight: ${x.weight}, edge: Edge(from: ${x.edge.from}, to: ${x.edge.to}))"
        )

      }
    }
  }
}

object ProductDerivationSpec {
  case class Edge( from: String, to: String )
  case class WeightedEdge( weight: Int, edge: Edge )

  val name = for {
    s  <- Gen.sized( Gen.const )
    cs <- Gen.listOfN( s max 1, Gen.alphaLowerChar )
  } yield cs.mkString

  val genEdge: Gen[Edge] = {

    ( name, name ).mapN( Edge )
  }

  val genWeightedEdge =
    ( arbitrary[Int], genEdge ).mapN( WeightedEdge )

}
