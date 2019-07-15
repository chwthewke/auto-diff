package fr.thomasdufour.autodiff

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

/**
  * *Law:* `nudge(ga, a)` can generate all values from ga, except those equal to `a`
  *
  * @tparam A
  */
trait Nudge[A] {
  def nudge( g: Gen[A], a: A ): Gen[A]
}

object Nudge extends LowPriorityNudgeImplicits {

  def apply[A]( implicit ev: Nudge[A] ): Nudge[A] = ev

  implicit def nudgeNumeric[A]( implicit N: Numeric[A] ): Nudge[A] =
    new Nudge[A] {
      override def nudge( g: Gen[A], a: A ): Gen[A] = g.map { b =>
        if (b == a) N.plus( a, N.one ) else b
      }
    }

  implicit val nudgeBool: Nudge[Boolean] =
    new Nudge[Boolean] {
      override def nudge( g: Gen[Boolean], a: Boolean ): Gen[Boolean] = !a
    }

  implicit val nudgeString: Nudge[String] =
    new Nudge[String] {
      override def nudge( g: Gen[String], a: String ): Gen[String] =
        for {
          b <- arbitrary[Boolean]
          s <- g
        } yield {
          if (s != a) s
          else if (a.length > 0 && b) a.substring( 0, a.length - 1 )
          else a + "_"
        }
    }

  def different[A: Nudge]( g: Gen[A] ): Gen[( A, A )] = {
    val N = Nudge[A]
    for {
      a <- g
      b <- N.nudge( g, a )
    } yield ( a, b )
  }

  implicit class NudgeGenOps[A]( val self: Gen[A] ) extends AnyVal {
    def except( a: A )( implicit N: Nudge[A] ): Gen[A] = N.nudge( self, a )
  }

}

trait LowPriorityNudgeImplicits {
  implicit def filteringNudge[A]: Nudge[A] =
    new Nudge[A] {
      override def nudge( g: Gen[A], a: A ): Gen[A] = g.suchThat( _ != a )
    }
}
