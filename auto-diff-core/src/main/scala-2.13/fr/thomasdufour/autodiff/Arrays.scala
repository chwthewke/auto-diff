package fr.thomasdufour.autodiff

import scala.collection.immutable.ArraySeq

private[autodiff] object Arrays {
  trait DiffImplicits {
    implicit def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]] =
      IndexedSeqDiff.indexedSeqDiff[A, ArraySeq[A]]( "Array" ).contramap( ArraySeq.unsafeWrapArray )
  }

  trait AsIterableImplicits {
    implicit val arrayAsIterable: AsIterable[Array] = new AsIterable[Array] {
      override def asIterable[A]( coll: Array[A] ): ArraySeq[A] = ArraySeq.unsafeWrapArray( coll )
    }
  }
}
