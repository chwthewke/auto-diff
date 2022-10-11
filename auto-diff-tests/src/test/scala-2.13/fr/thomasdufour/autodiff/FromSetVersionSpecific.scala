package fr.thomasdufour.autodiff

import scala.collection.Factory

trait FromSetVersionSpecific {
  implicit def factoryFromSet[A, C]( implicit fac: Factory[A, C] ): FromSet[A, C] =
    new FromSet[A, C] {
      override def fromSet( set: Set[A] ): C = set.to( fac )
    }
}
