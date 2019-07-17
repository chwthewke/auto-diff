package fr.thomasdufour.autodiff

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector

trait FromNonEmptyVector[A, C[_]] {
  def fromNev( nev: NonEmptyVector[A] ): C[A]
}

object FromNonEmptyVector {

  implicit def nonEmptyChainFromNonEmptyVector[A]: FromNonEmptyVector[A, NonEmptyChain] =
    new FromNonEmptyVector[A, NonEmptyChain] {
      override def fromNev( nev: NonEmptyVector[A] ): NonEmptyChain[A] = NonEmptyChain.fromNonEmptyVector( nev )
    }

  implicit def nonEmptyListFromNonEmptyVector[A]: FromNonEmptyVector[A, NonEmptyList] =
    new FromNonEmptyVector[A, NonEmptyList] {
      override def fromNev( nev: NonEmptyVector[A] ): NonEmptyList[A] = NonEmptyList.fromReducible( nev )
    }

  implicit def nonEmptyVectorFromNonEmptyVector[A]: FromNonEmptyVector[A, NonEmptyVector] =
    new FromNonEmptyVector[A, NonEmptyVector] {
      override def fromNev( nev: NonEmptyVector[A] ): NonEmptyVector[A] = nev
    }

  implicit def fromVector[A, F[_]]( implicit ev: FromVector[A, F] ): FromNonEmptyVector[A, F] = ev
}
