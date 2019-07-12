package fr.thomasdufour.autodiff

import cats.syntax.either._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DiffStdAdtSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import Shrink.shrinkAny
  import DiffOps._

  "an Option[A] diff" should {
    val diff: Diff[Option[Int]] = Diff[Option[Int]]

    "report differences inside Some" in {
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          diff( Some( x ), Some( y ) ).tree should ===(
            T( T.Coproduct, "Option", x.toString !== y.toString )
          )
      }
    }

    "report no differences between identical values" in {
      forAll( Gen.option( arbitrary[Int] ) ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "report a difference between None and a Some" in {
      forAll( arbitrary[Int] ) { x =>
        diff( Some( x ), None ).tree should ===( T( T.Coproduct, "Option", s"Some($x)" !== "None" ) )
      }
    }

    "report no difference between different Some if the implicit inner diff ignores it" in {
      implicit val diffInt: Diff[Int] = Diff.ignore[Int]
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          Diff[Option[Int]].apply( Some( x ), Some( y ) ).tree should ===( Z )
      }
    }

    "show Some values" in {
      forAll( arbitrary[Int] ) { x =>
        diff.show( Some( x ) ) should ===( s"Some($x)" )
      }
    }

    "show None" in {
      diff.show( None ) should ===( s"None" )
    }
  }

  "an Either[A, B] diff" should {
    val diff: Diff[Either[String, Int]] = Diff[Either[String, Int]]

    "report differences inside Right" in {
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          diff( Right( x ), Right( y ) ).tree should ===(
            T( T.Coproduct, "Right", x.toString !== y.toString )
          )
      }
    }

    "report differences inside Left" in {
      forAll( Nudge.different( arbitrary[String] ) ) {
        case ( x, y ) =>
          diff( Left( x ), Left( y ) ).tree should ===(
            T( T.Coproduct, "Left", x.toString !== y.toString )
          )
      }
    }

    "report no differences between identical Rights" in {
      forAll( arbitrary[Int].map( Either.right[String, Int]( _ ) ) ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "report no differences between identical Lefts" in {
      forAll( arbitrary[String].map( Either.left[String, Int]( _ ) ) ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "report a difference between Left and a Right" in {
      forAll( arbitrary[Int], arbitrary[String] ) { ( x, y ) =>
        diff( Right( x ), Left( y ) ).tree should ===( T( T.Coproduct, "Either", s"Right($x)" !== s"Left($y)" ) )
      }
    }

    "report no difference between different Rights if the implicit inner diff ignores it" in {
      implicit val diffInt: Diff[Int] = Diff.ignore[Int]
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          Diff[Either[String, Int]].apply( Right( x ), Right( y ) ).tree should ===( Z )
      }
    }

    "report no difference between different Lefts if the implicit inner diff ignores it" in {
      implicit val diffString: Diff[String] = Diff.ignore[String]
      forAll( Nudge.different( arbitrary[String] ) ) {
        case ( x, y ) =>
          Diff[Either[String, Int]].apply( Left( x ), Left( y ) ).tree should ===( Z )
      }
    }

    "show Right values" in {
      forAll( arbitrary[Int] ) { x =>
        diff.show( Right( x ) ) should ===( s"Right($x)" )
      }
    }

    "show Left values" in {
      forAll( arbitrary[String] ) { x =>
        diff.show( Left( x ) ) should ===( s"Left($x)" )
      }
    }
  }

}
