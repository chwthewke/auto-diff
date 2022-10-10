package fr.thomasdufour.autodiff

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object CatsDataDiff {

  def chainDiff[A]( implicit D: Diff[A] ): Diff[Chain[A]] =
    LinearSeqDiff.diffAsList[A]( "Chain" ).contramap( _.toList )

  def nonEmptyChainDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyChain[A]] =
    LinearSeqDiff.diffAsList[A]( "NonEmptyChain" ).contramap( _.toChain.toList )

  def nonEmptyListDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyList[A]] =
    LinearSeqDiff.diffAsList[A]( "NonEmptyList" ).contramap( _.toList )

  def nonEmptyVectorDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyVector[A]] =
    IndexedSeqDiff.indexedSeqDiff[A, Vector[A]]( "NonEmptyVector" ).contramap( _.toVector )

  def nonEmptySetDiff[A]( implicit D: Diff[A] ): Diff[NonEmptySet[A]] =
    SetDiff.setLikeDiff[A, SortedSet[A]]( "NonEmptySet" ).contramap( _.toSortedSet )

  def nonEmptyMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[NonEmptyMap[K, V]] =
    MapDiff.mapLikeDiff[K, V, SortedMap[K, V]]( "NonEmptyMap" ).contramap( _.toSortedMap )

}
