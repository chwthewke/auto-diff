package fr.thomasdufour.autodiff

import cats.data.Validated
import cats.data.Validated._
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

    "report a difference between a Left and a Right" in {
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

  "a Validated[A, B] diff" should {
    val diff: Diff[Validated[String, Int]] = Diff[Validated[String, Int]]

    "report differences inside Valid" in {
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          diff( Valid( x ), Valid( y ) ).tree should ===(
            T( T.Coproduct, "Valid", x.toString !== y.toString )
          )
      }
    }

    "report differences inside Invalid" in {
      forAll( Nudge.different( arbitrary[String] ) ) {
        case ( x, y ) =>
          diff( Invalid( x ), Invalid( y ) ).tree should ===(
            T( T.Coproduct, "Invalid", x.toString !== y.toString )
          )
      }
    }

    "report no differences between identical Valids" in {
      forAll( arbitrary[Int].map( Valid( _ ) ) ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "report no differences between identical Invalids" in {
      forAll( arbitrary[String].map( Invalid( _ ) ) ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

    "report a difference between an Invalid and a Valid" in {
      forAll( arbitrary[Int], arbitrary[String] ) { ( x, y ) =>
        diff( Valid( x ), Invalid( y ) ).tree should ===(
          T( T.Coproduct, "Validated", s"Valid($x)" !== s"Invalid($y)" )
        )
      }
    }

    "report no difference between different Valids if the implicit inner diff ignores it" in {
      implicit val diffInt: Diff[Int] = Diff.ignore[Int]
      forAll( Nudge.different( arbitrary[Int] ) ) {
        case ( x, y ) =>
          Diff[Validated[String, Int]].apply( Valid( x ), Valid( y ) ).tree should ===( Z )
      }
    }

    "report no difference between different Invalids if the implicit inner diff ignores it" in {
      implicit val diffString: Diff[String] = Diff.ignore[String]
      forAll( Nudge.different( arbitrary[String] ) ) {
        case ( x, y ) =>
          Diff[Validated[String, Int]].apply( Invalid( x ), Invalid( y ) ).tree should ===( Z )
      }
    }

    "show Valid values" in {
      forAll( arbitrary[Int] ) { x =>
        diff.show( Valid( x ) ) should ===( s"Valid($x)" )
      }
    }

    "show Invalid values" in {
      forAll( arbitrary[String] ) { x =>
        diff.show( Invalid( x ) ) should ===( s"Invalid($x)" )
      }
    }
  }

}
