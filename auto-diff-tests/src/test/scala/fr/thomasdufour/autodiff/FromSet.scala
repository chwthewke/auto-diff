package fr.thomasdufour.autodiff

import cats.data.NonEmptySet

trait FromNonEmptySet[A, C] {
  def fromNonEmptySet( set: NonEmptySet[A] ): C
}

object FromNonEmptySet {
  implicit def nonEmptySetFromNonEmptySet[A]: FromNonEmptySet[A, NonEmptySet[A]] =
    (set: NonEmptySet[A]) => set
}

trait FromSet[A, C] extends FromNonEmptySet[A, C] {
  def fromSet( set: Set[A] ): C

  override def fromNonEmptySet( set: NonEmptySet[A] ): C = fromSet( set.toSortedSet )
}

object FromSet extends FromSetVersionSpecific
