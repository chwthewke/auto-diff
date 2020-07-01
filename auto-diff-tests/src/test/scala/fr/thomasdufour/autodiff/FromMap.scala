package fr.thomasdufour.autodiff

trait FromMap[K, V, C] {
  def fromMap( map: Map[K, V] ): C
}

object FromMap extends FromMapVersionSpecific
