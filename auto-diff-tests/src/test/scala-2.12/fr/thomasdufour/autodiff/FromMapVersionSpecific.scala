package fr.thomasdufour.autodiff

import scala.collection.immutable.HashMap
import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.LongMap
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap

trait FromMapVersionSpecific {

  implicit def mapFromMap[K, V]: FromMap[K, V, Map[K, V]] =
    new FromMap[K, V, Map[K, V]] {
      override def fromMap( map: Map[K, V] ): Map[K, V] = map
    }

  implicit def sortedMapFromMap[K: Ordering, V]: FromMap[K, V, SortedMap[K, V]] =
    new FromMap[K, V, SortedMap[K, V]] {
      override def fromMap( map: Map[K, V] ): SortedMap[K, V] = SortedMap( map.toSeq: _* )
    }

  implicit def hashMapFromMap[K, V]: FromMap[K, V, HashMap[K, V]] =
    new FromMap[K, V, HashMap[K, V]] {
      override def fromMap( map: Map[K, V] ): HashMap[K, V] = HashMap( map.toSeq: _* )
    }

  implicit def listMapFromMap[K, V]: FromMap[K, V, ListMap[K, V]] =
    new FromMap[K, V, ListMap[K, V]] {
      override def fromMap( map: Map[K, V] ): ListMap[K, V] = ListMap( map.toSeq: _* )
    }

  implicit def fromMapSTreeMap[K: Ordering, V]: FromMap[K, V, TreeMap[K, V]] =
    new FromMap[K, V, TreeMap[K, V]] {
      override def fromMap( map: Map[K, V] ): TreeMap[K, V] = TreeMap( map.toSeq: _* )
    }

  implicit def fromMapIntMap[V]: FromMap[Int, V, IntMap[V]] =
    new FromMap[Int, V, IntMap[V]] {
      override def fromMap( map: Map[Int, V] ): IntMap[V] = IntMap( map.toSeq: _* )
    }

  implicit def fromMapLongMap[V]: FromMap[Long, V, LongMap[V]] =
    new FromMap[Long, V, LongMap[V]] {
      override def fromMap( map: Map[Long, V] ): LongMap[V] = LongMap( map.toSeq: _* )
    }

}
