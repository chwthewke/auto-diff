package fr.thomasdufour.autodiff
package extra

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

object scalatest {
  // TODO configure presence of expected, actual (via implicit?)

  def matchWithAutoDiff[A]( expected: A )( implicit D: Diff[A] ): Matcher[A] =
    new Matcher[A] {
      private def message( actual: A, difference: Difference ): String =
        s"""${Difference.Pretty.Plain2.show( difference )}
           |  expected: ${D.show( expected )}
           |  actual:   ${D.show( actual )}
         """.stripMargin

      override def apply( actual: A ): MatchResult =
        D( actual, expected )
          .fold( MatchResult( matches = true, "", "" ) )( d =>
            MatchResult( matches = false, message( actual, d ), "" ) )

      override def toString(): String =
        s"autodiff-equivalent to ${D.show( expected )}"
    }

  def ~=[A]( expected: A )( implicit D: Diff[A] ): Matcher[A] = matchWithAutoDiff( expected )

}
