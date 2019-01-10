package fr.thomasdufour.autodiff

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector

object CatsDataDiff {

  def chainDiff[A]( implicit D: Diff[A] ): Diff[Chain[A]] =
    LinearSeqDiff.diffLinearSeq[A, List[A]]( "Chain" ).contramap( _.toList )

  def nonEmptyChainDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyChain[A]] =
    LinearSeqDiff.diffLinearSeq[A, List[A]]( "NonEmptyChain" ).contramap( _.toChain.toList )

  def nonEmptyListDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyList[A]] =
    LinearSeqDiff.diffLinearSeq[A, List[A]]( "NonEmptyList" ).contramap( _.toList )

  def nonEmptyVectorDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyVector[A]] =
    IndexedSeqDiff.indexedSeqDiff[A, Vector[A]]( "NonEmptyVector" ).contramap( _.toVector )

}
