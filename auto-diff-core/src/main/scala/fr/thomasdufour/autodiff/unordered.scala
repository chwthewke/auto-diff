package fr.thomasdufour.autodiff

import cats.syntax.profunctor._
import shapeless.Lazy

object unordered {
  def inAnyOrder[A]( coll: Traversable[A] ): AnyOrder[A] = new AnyOrder[A]( coll )

  class AnyOrder[A]( val coll: Traversable[A] ) extends AnyVal

  def anyOrderDiffShow[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[AnyOrder[A], ValueDifference] =
    new DiffShow[AnyOrder[A]] {
      override def show( a: AnyOrder[A] ): String = matching.showUnordered( DiffShow.fromLazy( D ), a.coll )

      override def diff( left: AnyOrder[A], right: AnyOrder[A] ): Option[ValueDifference] =
        matching.diffMatch( left.coll, right.coll ).difference

      override type Out = ValueDifference
    }

  def setDiffShow[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[AnyOrder[A], D]] ): DiffShow.Aux[Set[A], TaggedDifference] =
    DiffShow
      .fromLazy( D )
      .dimap[Set[A], TaggedDifference]( set => new AnyOrder[A]( set ) )( diff => TaggedDifference( "Set ", diff ) )
}
