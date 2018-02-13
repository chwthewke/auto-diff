package fr.thomasdufour.autodiff

import cats.syntax.option._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class TupleSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {
  import DiffOps._

  "Diffing tuples" when {
    "containing primitive types" when {
      type T = ( Int, String, Unit )
      val D = Diff[T]

      "identical" should {
        "return no difference" in {
          D( ( 1, "abc", () ), ( 1, "abc", () ) ).tree should ===( Z )
        }
      }

      "different in the first position" should {
        "return the difference tagged as tuple with index 1" in {
          D( ( 1, "a", () ), ( 2, "a", () ) ).tree should ===(
            I( T.Tuple, "Tuple3", 1 -> ("1" !== "2") )
          )
        }
      }

      "different in the second position" should {
        "return the difference tagged as tuple with index 1" in {
          D( ( 0, "a", () ), ( 0, "bc", () ) ).tree should ===(
            I( T.Tuple, "Tuple3", 2 -> ("a" !== "bc") )
          )
        }
      }

      "different in the both the first and second position" should {
        "return the difference tagged as tuple with both differences" in {
          D( ( 1, "a", () ), ( 0, "bc", () ) ).tree should ===(
            I( T.Tuple, "Tuple3", 1 -> ("1" !== "0"), 2 -> ("a" !== "bc") )
          )
        }
      }
    }

    "having nested tuples" when {
      type T = ( ( Int, String ), Boolean )

      val D = Diff[T]

      "identical" should {
        "return no difference" in {
          D( ( ( 1, "ab" ), true ), ( ( 1, "ab" ), true ) ).tree should ===( Z )
        }
      }

      "different in outer tuple" should {
        "return the difference at depth 1" in {

          D( ( ( 1, "ab" ), true ), ( ( 1, "ab" ), false ) ).tree should ===(
            I( T.Tuple, "Tuple2", 2 -> ("true" !== "false") )
          )

        }
      }

      "different in inner tuple" should {
        "return the difference at depth 2" in {
          D( ( ( 1, "ab" ), true ), ( ( 2, "ab" ), true ) ).tree should ===(
            I( T.Tuple, "Tuple2", 1 -> I( T.Tuple, "Tuple2", 1 -> ("1" !== "2") ) )
          )
        }
      }

      "different in inner tuple" should {
        "return both differences" in {
          D( ( ( 1, "ab" ), true ), ( ( 2, "ab" ), false ) ).tree should ===(
            I( T.Tuple, "Tuple2", 1 -> I( T.Tuple, "Tuple2", 1 -> ("1" !== "2") ), 2 -> ("true" !== "false") )
          )
        }
      }
    }

    "containing composite types" should {
      type T = ( Option[String], Int )

      val D = Diff[T]

      "wrap any inner difference" in {
        D( ( "abc".some, 1 ), ( none, 1 ) ).tree should ===(
          I( T.Tuple, "Tuple2", 1 -> T( T.Coproduct, "Option", "Some(abc)" !== "None" ) )
        )
      }
    }
  }
}
