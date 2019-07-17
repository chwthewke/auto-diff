package fr.thomasdufour.autodiff

import scala.collection.immutable.ArraySeq

trait DiffVersionSpecific {
  implicit def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]] =
    IndexedSeqDiff.indexedSeqDiff[A, ArraySeq[A]]( "Array" ).contramap( ArraySeq.unsafeWrapArray )

  implicit def arraySeqDiff[A]( implicit D: Diff[A] ): Diff[ArraySeq[A]] =
    IndexedSeqDiff.indexedSeqDiff[A, ArraySeq[A]]( "ArraySeq" )

  implicit def lazyListDiff[A]( implicit D: Diff[A] ): Diff[LazyList[A]] =
    LinearSeqDiff.diffAsList[A]( "LazyList" ).contramap( _.toList )
}
