package fr.thomasdufour.autodiff
package derived

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

}

object RecursiveParametricAdtSpec {
  sealed abstract class AList[A]
  final case class ANil[A]()                           extends AList[A]
  final case class ACons[A]( head: A, tail: AList[A] ) extends AList[A]
}
