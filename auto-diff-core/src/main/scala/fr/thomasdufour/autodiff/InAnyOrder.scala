package fr.thomasdufour.autodiff

import cats.arrow.FunctionK
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector

class InAnyOrder[A]( val coll: Iterable[A] ) extends AnyVal

object InAnyOrder {
  def apply[A]( coll: Iterable[A] ): InAnyOrder[A] = new InAnyOrder( coll )

  def diffable[F[_], A]( coll: F[A] )( implicit D: Diffable[F] ): InAnyOrder[A] =
    InAnyOrder[A]( D.toIterable( coll ) )

  implicit def anyOrderDiff[A]( implicit D: Diff[A], H: DiffMatch.Hint[A] ): Diff[InAnyOrder[A]] =
    new Diff[InAnyOrder[A]] {
      override def show( a: InAnyOrder[A] ): String = "{ " + DiffMatch.showUnordered( D, a.coll ) + " }"

      override def apply( left: InAnyOrder[A], right: InAnyOrder[A] ): Option[Difference] =
        DiffMatch.of( left.coll, right.coll ).difference

    }

  trait Diffable[F[_]] {
    def toIterable[A]( coll: F[A] ): Iterable[A]
  }

  object Diffable {
    def apply[F[_]]( implicit ev: Diffable[F] ): Diffable[F] = ev

    def instance[F[_], CC[x] <: Iterable[x]]( f: FunctionK[F, CC] ): Diffable[F] =
      new Diffable[F] {
        override def toIterable[A]( coll: F[A] ): Iterable[A] = f( coll )
      }

    implicit def diffableInAnyOrderCollection[F[x] <: Iterable[x]] = new Diffable[F] {
      override def toIterable[A]( coll: F[A] ): Iterable[A] = coll
    }

    implicit val diffableChain: Diffable[Chain] = new Diffable[Chain] {
      override def toIterable[A]( coll: Chain[A] ): Iterable[A] = coll.toList
    }

    implicit val diffableNonEmptyChain: Diffable[NonEmptyChain] = new Diffable[NonEmptyChain] {
      override def toIterable[A]( coll: NonEmptyChain[A] ): Iterable[A] = coll.toChain.toList
    }

    implicit val diffableNonEmptyList: Diffable[NonEmptyList] = new Diffable[NonEmptyList] {
      override def toIterable[A]( coll: NonEmptyList[A] ): Iterable[A] = coll.toList
    }

    implicit val diffableNonEmptyVector: Diffable[NonEmptyVector] = new Diffable[NonEmptyVector] {
      override def toIterable[A]( coll: NonEmptyVector[A] ): Iterable[A] = coll.toVector
    }

  }
}
