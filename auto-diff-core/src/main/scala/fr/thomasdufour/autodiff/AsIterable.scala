package fr.thomasdufour.autodiff

import cats.Foldable

trait AsIterable[F[_]] {
  def asIterable[A]( coll: F[A] ): Iterable[A]
}

object AsIterable extends AsIterableVersionSpecific with AsIterableLowPriority {
  def apply[F[_]]( implicit ev: AsIterable[F] ): AsIterable[F] = ev

  implicit def collectionAsIterable[F[x] <: Iterable[x]]: AsIterable[F] = new AsIterable[F] {
    override def asIterable[A]( coll: F[A] ): Iterable[A] = coll
  }

  private[autodiff] implicit class AsIterableOps[F[_], A]( val self: F[A] ) extends AnyVal {
    def asIterable( implicit F: AsIterable[F] ): Iterable[A] = F.asIterable( self )
    def size( implicit F: AsIterable[F] ): Int               = F.asIterable( self ).size
    def at( ix: Int )( implicit F: AsIterable[F] ): A        = F.asIterable( self ).toVector.apply( ix )
  }
}

trait AsIterableLowPriority {
  implicit def foldableAsIterable[F[_]]( implicit F: Foldable[F] ): AsIterable[F] =
    new AsIterable[F] {
      override def asIterable[A]( coll: F[A] ): Iterable[A] = F.toList( coll )
    }
}
