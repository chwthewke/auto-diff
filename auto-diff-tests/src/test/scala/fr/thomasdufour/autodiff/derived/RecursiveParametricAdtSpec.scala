package fr.thomasdufour.autodiff
package derived

import cats.Eval
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class RecursiveParametricAdtSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import RecursiveParametricAdtSpec._
  import DiffOps._

  "Deriving a Diff for a recursive case class" which {
    "has recursion under a custom ADT" should {
      val diff: Diff[AList[Int]] = {
        import auto._
        semi.diff
      }

      "compute a nested value diff" in {

        diff( ACons( 1, ACons( 2, ANil() ) ), ACons( 1, ACons( 3, ANil() ) ) ).tree should ===(
          T(
            T.Coproduct,
            "AList",
            F( "ACons", "tail" -> T( T.Coproduct, "AList", F( "ACons", "head" -> ("2" !== "3") ) ) )
          )
        )

      }

      "compute a nested structure diff" in {

        diff( ACons( 1, ACons( 2, ANil() ) ), ACons( 1, ANil() ) ).tree should ===(
          T(
            T.Coproduct,
            "AList",
            F( "ACons", "tail" -> T( T.Coproduct, "AList", "ACons(...)" !== "ANil(...)" ) )
          )
        )

      }

    }
  }

  "Deriving a Diff for a Cofree (of an admissible functor)" should {
    val diff: Diff[Cofree[List, Int]] = {
      import auto._
      semi.diff
    }

    val nil: Eval[List[Cofree[List, Int]]] = Eval.now( List[Cofree[List, Int]]() )

    "report a difference in the head" in {
      diff(
        Cofree( 1, Eval.later( List( Cofree( 2, nil ) ) ) ),
        Cofree( 2, Eval.later( List( Cofree( 2, nil ) ) ) )
      ).tree should ===(
        F( "Cofree", "head" -> ("1" !== "2") )
      )
    }

    "report a difference in the tail" in {
      diff(
        Cofree( 1, Eval.later( List( Cofree( 2, nil ), Cofree( 5, nil ) ) ) ),
        Cofree( 1, Eval.later( List( Cofree( 3, nil ), Cofree( 5, nil ) ) ) )
      ).tree should ===(
        F( "Cofree", "tail" -> I( T.Seq, "List", 0 -> F( "Cofree", "head" -> ("2" !== "3") ) ) )
      )
    }

  }

  "Deriving a Diff for a Fix (of an admissible functor)" should {
    val diff: Diff[Fix[List]] = {
      import auto._
      semi.diff
    }

    val nil: Eval[List[Fix[List]]] = Eval.now( List.empty )

    "report a difference" in {
      diff(
        Fix( nil ),
        Fix( Eval.later( List( Fix( nil ) ) ) )
      ).tree should ===(
        F( "Fix", "unfix" -> I( T.Seq, "List", 0 -> ("<end>" !== "Fix(unfix: List())") ) )
      )
    }

  }

}

object RecursiveParametricAdtSpec {
  sealed abstract class AList[A]
  final case class ANil[A]()                           extends AList[A]
  final case class ACons[A]( head: A, tail: AList[A] ) extends AList[A]

  final case class Cofree[S[_], A]( head: A, tail: Eval[S[Cofree[S, A]]] )

  final case class Fix[F[_]]( unfix: Eval[F[Fix[F]]] )
}
