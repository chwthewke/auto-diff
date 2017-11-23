package fr.thomasdufour.autodiff
package extra

import cats.syntax.show._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

object scalatest {
  def matchWithAutoDiff[A]( expected: A )( implicit diff: Diff[A] ): Matcher[A] =
    new Matcher[A] {
      override def apply( actual: A ): MatchResult =
        diff( actual, expected )
          .fold( MatchResult( matches = true, "", "" ) )( d => MatchResult( matches = false, d.show, "" ) )

      override def toString(): String =
        s"autodiff-equivalent to ${diff.show( expected )}"
    }

}
