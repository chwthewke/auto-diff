package fr.thomasdufour.autodiff.scalatest

import org.scalatest.Inside
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatest.exceptions.TestFailedException
import scala.language.implicitConversions
import fr.thomasdufour.autodiff.extra.scalatest.AutodiffMatchers._

class ScalatestMatcherSpec extends WordSpec with Matchers with Inside {

  "Testing a succeeding diff" when {
    "using the regular function" should {
      "pass" in {
        1 should matchWithAutoDiff( 1 )
      }
    }

    "using the operator" should {
      "pass" in {
        1 should ~=( 1 )
      }
    }
  }

  implicit def ToFailOps[A]( a: => A ) =
    new FailOps[A]( () => a )

  class FailOps[A]( val self: () => A ) {
    def failsStartingWith( messageInit: String ): Unit =
      inside( the[TestFailedException] thrownBy (self() ) ) {
        case ex => ex.getMessage should startWith( messageInit )
      }
  }

  "Testing a failing diff" when {
    "using the regular function" should {
      "fail" in {
        (1 should matchWithAutoDiff( 2 )) failsStartingWith ("1 -> 2")
      }
    }

    "using the operator" should {
      "fail" in {
        (1 should ~=( 2 )) failsStartingWith ("1 -> 2")
      }
    }
  }

}
