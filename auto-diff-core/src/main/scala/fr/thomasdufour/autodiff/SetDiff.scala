package fr.thomasdufour.autodiff

import scala.collection.SetLike
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListSet
import scala.collection.immutable.TreeSet

object SetDiff {

  def setLikeDiff[A, CC <: SetLike[A, CC] with Set[A]]( name: String )( implicit D: Diff[A] ): Diff[CC] =
    Diff[InAnyOrder[A]]
      .contramap[CC]( set => new InAnyOrder[A]( set ) )
      .mapDifference( d => Difference.Set( name, d ) )

  def setDiff[A]( implicit D: Diff[A] ): Diff[Set[A]]         = setLikeDiff[A, Set[A]]( "Set" )
  def listSetDiff[A]( implicit D: Diff[A] ): Diff[ListSet[A]] = setLikeDiff[A, ListSet[A]]( "ListSet" )
  def hashSetDiff[A]( implicit D: Diff[A] ): Diff[HashSet[A]] = setLikeDiff[A, HashSet[A]]( "HashSet" )
  def treeSetDiff[A]( implicit D: Diff[A] ): Diff[TreeSet[A]] = setLikeDiff[A, TreeSet[A]]( "TreeSet" )
}
