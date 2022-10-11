package fr.thomasdufour.autodiff

import cats.data.NonEmptyMap

trait FromNonEmptyMap[K, V, C] {
  def fromNonEmptyMap( map: NonEmptyMap[K, V] ): C
}

object FromNonEmptyMap {
  implicit def nonEmptyMapFromNonEmptyMap[K, V]: FromNonEmptyMap[K, V, NonEmptyMap[K, V]] =
    (map: NonEmptyMap[K, V]) => map
}

trait FromMap[K, V, C] extends FromNonEmptyMap[K, V, C] {
  def fromMap( map: Map[K, V] ): C

  override def fromNonEmptyMap( map: NonEmptyMap[K, V] ): C =
    fromMap( map.toSortedMap )
}

object FromMap extends FromMapVersionSpecific
