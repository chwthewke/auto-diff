package fr.thomasdufour.autodiff

import scala.collection.immutable.ArraySeq

trait AsIterableVersionSpecific {
  implicit val arrayAsIterable: AsIterable[Array] = new AsIterable[Array] {
    override def asIterable[A]( coll: Array[A] ): ArraySeq[A] = ArraySeq.unsafeWrapArray( coll )
  }
}
