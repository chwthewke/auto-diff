package fr.thomasdufour.autodiff

import scala.collection.immutable.HashSet
import scala.collection.immutable.ListSet
import scala.collection.immutable.TreeSet

private[autodiff] object SetDiff {

  def setLikeDiff[A, CC <: Set[A]](
      name: String
  )( implicit D: Diff[A], H: Hint[A] ): Diff[CC] =
    InAnyOrder
      .mkAnyOrderDiff( d => Difference.Set( name, d ) )
      .contramap[CC]( set => new InAnyOrder[A]( set ) )

  def setDiff[A]( implicit D: Diff[A] ): Diff[Set[A]]         = setLikeDiff[A, Set[A]]( "Set" )
  def listSetDiff[A]( implicit D: Diff[A] ): Diff[ListSet[A]] = setLikeDiff[A, ListSet[A]]( "ListSet" )
  def hashSetDiff[A]( implicit D: Diff[A] ): Diff[HashSet[A]] = setLikeDiff[A, HashSet[A]]( "HashSet" )
  def treeSetDiff[A]( implicit D: Diff[A] ): Diff[TreeSet[A]] = setLikeDiff[A, TreeSet[A]]( "TreeSet" )
}
