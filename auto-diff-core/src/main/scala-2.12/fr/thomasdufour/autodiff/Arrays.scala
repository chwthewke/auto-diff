package fr.thomasdufour.autodiff

import scala.collection.mutable

private[autodiff] object Arrays {
  trait DiffImplicits {
    implicit def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]] =
      IndexedSeqDiff.indexedSeqDiff[A, Vector[A]]( "Array" ).contramap( _.toVector )
  }

  trait AsIterableImplicits {
    implicit val arrayAsIterable: AsIterable[Array] = new AsIterable[Array] {
      override def asIterable[A]( coll: Array[A] ): mutable.WrappedArray[A] = coll
    }
  }
}
