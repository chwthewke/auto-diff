package fr.thomasdufour.autodiff

class InAnyOrder[A]( val coll: Iterable[A] ) extends AnyVal

object InAnyOrder {
  def apply[A]( coll: Iterable[A] ): InAnyOrder[A] = new InAnyOrder( coll )

  implicit def anyOrderDiff[A]( implicit D: Diff[A] ): Diff[InAnyOrder[A]] =
    new Diff[InAnyOrder[A]] {
      override def show( a: InAnyOrder[A] ): String = DiffMatch.showUnordered( "" )( D, a.coll )

      override def apply( left: InAnyOrder[A], right: InAnyOrder[A] ): Option[Difference] =
        DiffMatch.of( left.coll, right.coll ).difference

    }
}
