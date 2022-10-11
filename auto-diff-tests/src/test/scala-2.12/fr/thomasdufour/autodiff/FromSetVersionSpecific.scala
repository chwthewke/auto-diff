package fr.thomasdufour.autodiff

import scala.collection.immutable.SortedSet
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListSet
import scala.collection.immutable.TreeSet

trait FromSetVersionSpecific {
  implicit def setFromSet[A]: FromSet[A, Set[A]] =
    new FromSet[A, Set[A]] {
      override def fromSet( set: Set[A] ): Set[A] = set
    }

  implicit def sortedSetFromSet[A: Ordering]: FromSet[A, SortedSet[A]] =
    new FromSet[A, SortedSet[A]] {
      override def fromSet( set: Set[A] ): SortedSet[A] = SortedSet( set.toSeq: _* )
    }

  implicit def listSetFromSet[A]: FromSet[A, ListSet[A]] =
    new FromSet[A, ListSet[A]] {
      override def fromSet( set: Set[A] ): ListSet[A] = ListSet( set.toSeq: _* )
    }

  implicit def hashSetFromSet[A]: FromSet[A, HashSet[A]] =
    new FromSet[A, HashSet[A]] {
      override def fromSet( set: Set[A] ): HashSet[A] = HashSet( set.toSeq: _* )
    }

  implicit def treeSetFromSet[A: Ordering]: FromSet[A, TreeSet[A]] =
    new FromSet[A, TreeSet[A]] {
      override def fromSet( set: Set[A] ): TreeSet[A] = TreeSet( set.toSeq: _* )
    }
}
