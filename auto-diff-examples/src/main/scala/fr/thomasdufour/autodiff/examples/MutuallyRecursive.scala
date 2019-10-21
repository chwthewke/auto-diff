package fr.thomasdufour.autodiff.examples

object MutuallyRecursive {
  import fr.thomasdufour.autodiff.Diff
  import fr.thomasdufour.autodiff.derived.auto
  import fr.thomasdufour.autodiff.derived.semi

  case class Outer( inners: Vector[Inner] )
  case class Inner( outers: Vector[Outer] )

  // BAD!! DON'T DO THIS
  object implicits1 {
    lazy implicit val outerDiff: Diff[Outer] = semi.diff[Outer]
    lazy implicit val innerDiff: Diff[Inner] = semi.diff[Inner]
  }

  // DO THIS INSTEAD
  trait implicits2 {
    protected def mkOuterDiff: Diff[Outer] = {
      import auto._
      semi.diff[Outer]
    }

    protected def mkInnerDiff: Diff[Inner] = {
      import auto._
      semi.diff[Inner]
    }
  }

  object implicits2 extends implicits2 {
    implicit val outerDiff: Diff[Outer] = mkOuterDiff
    implicit val innerDiff: Diff[Inner] = mkInnerDiff
  }
}
