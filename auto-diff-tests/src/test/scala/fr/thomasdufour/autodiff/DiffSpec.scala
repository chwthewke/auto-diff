package fr.thomasdufour.autodiff

import cats.Eq
import cats.Show
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DiffSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import org.scalacheck.Shrink.shrinkAny
  import DiffOps._

  def mod16( x: Int ): Int           = (x % 16 + 16) % 16
  def eqv( x: Int, y: Int ): Boolean = mod16( x ) == mod16( y )
  def show( x: Int )                 = s"the number $x"

  val neqvInts = for {
    x <- arbitrary[Int]
    y <- arbitrary[Int]
  } yield ( x, if (eqv( x, y )) y + 1 else y )

  val eqvInts = for {
    x <- arbitrary[Int]
    y <- arbitrary[Int]
  } yield ( x, y, y - mod16( y ) + mod16( x ) )

  def aDiffFromMod16Eq( diff: Diff[Int] ): Unit = {
    "determine differences with the eqv function" when {
      "it deems values equal" in {
        forAll( eqvInts ) {
          case ( x, _, y ) =>
            diff( x, y ).tree should ===( Z )
        }

      }

      "it deems values different" in {
        forAll( neqvInts ) {
          case ( x, y ) =>
            diff( x, y ).tree should !==( Z )
        }
      }
    }

    "report differences with the show function" in {

      forAll( neqvInts ) {
        case ( x, y ) =>
          diff( x, y ).tree should ===( show( x ) !== show( y ) )
      }

    }

    "show values with the show function" in {
      forAll( arbitrary[Int] ) { x =>
        diff.show( x ) should ===( show( x ) )
      }
    }

  }

  "A diff created via explicit functions" should {
    val diff: Diff[Int] = Diff.explicitEqShow( eqv, show )

    behave like aDiffFromMod16Eq( diff )
  }

  "A diff created via implicit Eq and Show" should {
    implicit val intEq: Eq[Int]     = Eq.instance( eqv )
    implicit val intShow: Show[Int] = Show.show( show )

    val diff: Diff[Int] = Diff.implicitEqShow

    behave like aDiffFromMod16Eq( diff )
  }

}
