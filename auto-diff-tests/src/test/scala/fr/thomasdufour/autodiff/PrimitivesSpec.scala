package fr.thomasdufour.autodiff

import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class PrimitivesSpec extends WordSpec with Matchers with TypeCheckedTripleEquals with GeneratorDrivenPropertyChecks {

  import DiffOps._

  def noDifference[A: Gen]( d: Diff[A] ): Unit =
    "return no difference" in {
      forAll( identical[A] ) {
        case ( left, right ) =>
          d( left, right ).tree should ===( Z )
      }
    }

  def aSimpleDifference[A: Gen: Nudge]( d: Diff[A] ): Unit =
    "return a simple difference" in {
      forAll( different[A] ) {
        case ( left, right ) =>
          d( left, right ).tree should ===( left.toString !== right.toString )
      }
    }

  def identical[A]( implicit g: Gen[A] ): Gen[( A, A )] = g.map( x => ( x, x ) )
  def different[A: Nudge]( implicit g: Gen[A] ): Gen[( A, A )] = {
    val N = Nudge[A]
    for {
      a <- g
      b <- N.nudge( g, a )
    } yield ( a, b )
  }

  def diffingArbitrary[A: Nudge: Arbitrary]( d: Diff[A] ): Unit = {
    implicit val gen = implicitly[Arbitrary[A]].arbitrary

    "identical" should {
      behave like noDifference( d )
    }

    "different" should {
      behave like aSimpleDifference( d )
    }
  }

  "Diffing longs" when {
    behave like diffingArbitrary( Diff[Long] )
  }

  "Diffing ints" when {
    behave like diffingArbitrary( Diff[Int] )
  }

  "Diffing shorts" when {
    behave like diffingArbitrary( Diff[Short] )
  }

  "Diffing bytes" when {
    behave like diffingArbitrary( Diff[Byte] )
  }

  "Diffing doubles" when {
    behave like diffingArbitrary( Diff[Double] )
  }

  "Diffing floats" when {
    behave like diffingArbitrary( Diff[Float] )
  }

  "Diffing booleans" when {
    behave like diffingArbitrary( Diff[Boolean] )
  }

  "Diffing chars" when {
    behave like diffingArbitrary( Diff[Char] )
  }

  "Diffing strings" when {
    behave like diffingArbitrary( Diff[String] )
  }

  "Diffing unit" should {
    val D = Diff[Unit]
    "return no difference" in {
      forAll( arbitrary[Unit], arbitrary[Unit] ) { ( a, b ) =>
        D( a, b ).tree should ===( Z )
      }
    }
  }

}
