package fr.thomasdufour.autodiff

import scala.collection.Factory

trait FromMapVersionSpecific {
  implicit def factoryFromMap[K, V, M]( implicit fac: Factory[( K, V ), M] ): FromMap[K, V, M] =
    new FromMap[K, V, M] {
      override def fromMap( map: Map[K, V] ): M = map.to( fac )
    }
}
