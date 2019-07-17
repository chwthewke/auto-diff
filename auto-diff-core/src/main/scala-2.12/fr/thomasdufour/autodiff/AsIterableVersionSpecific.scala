package fr.thomasdufour.autodiff

import scala.collection.mutable

trait AsIterableVersionSpecific {
  implicit val arrayAsIterable: AsIterable[Array] = new AsIterable[Array] {
    override def asIterable[A]( coll: Array[A] ): mutable.WrappedArray[A] = coll
  }
}
