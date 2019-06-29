package fr.thomasdufour.autodiff

import cats.Foldable

class InAnyOrder[A]( val coll: Iterable[A] ) extends AnyVal

object InAnyOrder {
  def diffable[F[_], A]( coll: F[A] )( implicit D: Diffable[F] ): InAnyOrder[A] =
    new InAnyOrder[A]( D.toIterable( coll ) )

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

  trait Diffable[F[_]] {
    def toIterable[A]( coll: F[A] ): Iterable[A]
  }

  object Diffable extends DiffableLowPriority {
    def apply[F[_]]( implicit ev: Diffable[F] ): Diffable[F] = ev

    implicit def diffableInAnyOrderCollection[F[x] <: Iterable[x]]: Diffable[F] = new Diffable[F] {
      override def toIterable[A]( coll: F[A] ): Iterable[A] = coll
    }

  }

  trait DiffableLowPriority {
    implicit def diffableFromFoldable[F[_]]( implicit F: Foldable[F] ): Diffable[F] =
      new Diffable[F] {
        override def toIterable[A]( coll: F[A] ): Iterable[A] = F.toList( coll )
      }
  }
}
