package fr.thomasdufour.autodiff

class InAnyOrder[A]( val coll: Iterable[A] ) extends AnyVal

object InAnyOrder {
  def diffable[F[_], A]( coll: F[A] )( implicit D: AsIterable[F] ): InAnyOrder[A] =
    new InAnyOrder[A]( D.asIterable( coll ) )

  private[autodiff] def unorderedDiff[A: Diff: DiffMatch.Hint](
      left: InAnyOrder[A],
      right: InAnyOrder[A]
  ): Option[Difference.Unordered] =
    DiffMatch.of( left.coll, right.coll ).difference

  def mkAnyOrderDiff[A, D <: Difference](
      cont: Difference.Unordered => D
  )( implicit D: Diff[A], H: DiffMatch.Hint[A] ): Diff[InAnyOrder[A]] =
    new Diff[InAnyOrder[A]] {
      override def apply( left: InAnyOrder[A], right: InAnyOrder[A] ): Option[Difference] =
        unorderedDiff( left, right ).map( cont )

      override def show( value: InAnyOrder[A] ): String = "{ " + DiffMatch.showUnordered( D, value.coll ) + " }"
    }

  implicit def anyOrderDiff[A]( implicit D: Diff[A], H: DiffMatch.Hint[A] ): Diff[InAnyOrder[A]] =
    mkAnyOrderDiff( identity )

}
