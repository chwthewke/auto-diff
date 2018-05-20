package fr.thomasdufour.autodiff

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class InAnyOrderSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import DiffOps._

  "Matching collections in any order" when {
    "without a match hint" when {

      val diff: Diff[Vector[Int]] = Diff.inAnyOrder[Int, Vector]

      "the collections are equal" should {
        "report no difference" in {

          diff.apply( Vector( 1, 2, 3 ), Vector( 1, 2, 3 ) ).tree should ===( Z )

        }
      }

      "the collections are equivalent" should {
        "report no difference" in {

          diff.apply( Vector( 1, 2, 3 ), Vector( 3, 1, 2 ) ).tree should ===( Z )

        }
      }

      "the collections have mismatched elements" should {
        "report the removed and added elements in order" in {

          diff.apply( Vector( 1, 3, 7 ), Vector( 0, 3, 5 ) ).tree should ===( U( Some( V( "1, 7", "0, 5" ) ), Nil ) )

        }
      }

      "the collections a single mismatched element last of several" should {
        "report the removed and added elements in order" in {

          diff.apply( Vector( 1, 3, 7 ), Vector( 1, 3, 5 ) ).tree should ===( U( Some( V( "7", "5" ) ), Nil ) )

        }
      }

      "the collections a single mismatched element first of several" should {
        "report the removed and added elements in order" in {

          diff.apply( Vector( 0, 3, 5 ), Vector( 1, 3, 5 ) ).tree should ===( U( Some( V( "0", "1" ) ), Nil ) )

        }
      }

    }

    "with a match hint" when {

      import cats.instances.int._
      implicit val hint: DiffMatch.Hint[( Int, String )] = DiffMatch.Hint.byEqShow[Int].contramap( _._1 )

      val diff = Diff.inAnyOrder[( Int, String ), Vector]

      "the collections are equivalent" should {
        "report no difference" in {

          diff.apply( Vector( 1 -> "a", 3 -> "c", 7 -> "g" ), Vector( 3 -> "c", 7 -> "g", 1 -> "a" ) ).tree should ===(
            Z )

        }
      }

      "the collections are equivalent with non-trivial hints" should {
        "report no difference" in {

          diff.apply( Vector( 1 -> "a", 1 -> "c" ), Vector( 1 -> "c", 1 -> "a" ) ).tree should ===( Z )

        }
      }

      "the collections have hinted mismatches" should {
        "report those in pairs" in {

          diff.apply( Vector( 1 -> "a", 2 -> "b" ), Vector( 1 -> "b", 2 -> "c" ) ).tree should ===(
            U( None,
              T( T.Gen, "at 1", I( T.Tuple, "Tuple2", ( 2, V( "a", "b" ) ) ) ) ::
                T( T.Gen, "at 2", I( T.Tuple, "Tuple2", ( 2, V( "b", "c" ) ) ) ) :: Nil )
          )

        }
      }

      "the collections have unhinted mismatches" should {
        "report those as added and removed" in {

          diff.apply( Vector( 5 -> "c", 1 -> "a", 2 -> "b" ), Vector( 1 -> "a", 4 -> "z" ) ).tree should ===(
            U( Some( V( "(5, c), (2, b)", "(4, z)" ) ), Nil )
          )

        }
      }

    }
  }

}