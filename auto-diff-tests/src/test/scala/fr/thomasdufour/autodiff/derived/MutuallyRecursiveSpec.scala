package fr.thomasdufour.autodiff
package derived

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class MutuallyRecursiveSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import MutuallyRecursiveSpec._
  import DiffOps._

  "Deriving a diff for mutually recursive case classes" when {
    "using semi" should {
      val diff: Diff[Outer] = {
        import auto._
        semi.diff[Outer]
      }

      "compute an outer diff (A)" in {
        diff( Outer( "foo", Vector.empty ), Outer( "bar", Vector.empty ) ).tree should ===(
          F( "Outer", "tag" -> ("foo" !== "bar") )
        )
      }

      "compute an inner diff (A-B)" in {
        diff( Outer( "foo", Vector( Inner( "bar", None ) ) ), Outer( "foo", Vector( Inner( "baz", None ) ) ) ).tree should ===(
          F( "Outer", "inners" -> I( T.Seq, "Vector", 0 -> F( "Inner", "tag" -> ("bar" !== "baz") ) ) )
        )
      }

      "compute a nested outer diff (A-B-A)" in {
        diff(
          Outer( "foo", Vector( Inner( "bar", Some( Outer( "baz", Vector.empty ) ) ) ) ),
          Outer( "foo", Vector( Inner( "bar", Some( Outer( "quux", Vector.empty ) ) ) ) )
        ).tree should ===(
          F(
            "Outer",
            "inners" -> I(
              T.Seq,
              "Vector",
              0 -> F(
                "Inner",
                "outer" -> T( T.Coproduct, "Option", F( "Outer", "tag" -> ("baz" !== "quux") ) )
              )
            )
          )
        )
      }
    }
  }

  "Using both diffs implicitly" should {

    import MutuallyRecursiveSpec.implicits._

    "compute an outer diff (A)" in {
      Diff[Outer].apply( Outer( "foo", Vector.empty ), Outer( "bar", Vector.empty ) ).tree should ===(
        F( "Outer", "tag" -> ("foo" !== "bar") )
      )
    }

    "compute an inner diff (A-B)" in {
      Diff[Outer]
        .apply( Outer( "foo", Vector( Inner( "bar", None ) ) ), Outer( "foo", Vector( Inner( "baz", None ) ) ) )
        .tree should ===(
        F( "Outer", "inners" -> I( T.Seq, "Vector", 0 -> F( "Inner", "tag" -> ("bar" !== "baz") ) ) )
      )
    }

  }

}

object MutuallyRecursiveSpec {
  case class Outer( tag: String, inners: Vector[Inner] )
  case class Inner( tag: String, outer: Option[Outer] )

  object implicits extends implicitsLow {
    implicit val innerDiff: Diff[Inner] = innerDiff0
    implicit val outerDiff: Diff[Outer] = outerDiff0
  }

  trait implicitsLow {

    val innerDiff0: Diff[Inner] = {
      import auto._
      semi.diff[Inner]
    }

    val outerDiff0: Diff[Outer] = {
      import auto._
      semi.diff[Outer]
    }

  }

}
