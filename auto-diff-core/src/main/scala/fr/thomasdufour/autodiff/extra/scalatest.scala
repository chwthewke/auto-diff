package fr.thomasdufour.autodiff
package extra

import cats.syntax.show._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

object scalatest {
  def matchWithAutoDiff[A]( expected: A )( implicit D: Diff[A] ): Matcher[A] =
    new Matcher[A] {
      private def message( actual: A, difference: Difference ): String =
        s"""${difference.show}
           |  expected: ${D.show( expected )}
           |  actual:   ${D.show( actual )}
         """.stripMargin

      override def apply( actual: A ): MatchResult =
        D.diff( actual, expected )
          .fold( MatchResult( matches = true, "", "" ) )( d =>
            MatchResult( matches = false, message( actual, d ), "" ) )

      override def toString(): String =
        s"autodiff-equivalent to ${D.show( expected )}"
    }

}
