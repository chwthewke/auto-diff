package fr.thomasdufour.autodiff

import cats.data.Ior
import cats.data.NonEmptyList

import scala.collection.immutable._

object MapDiff {

  def mapLikeDiff[K, V, CC <: MapLike[K, V, CC] with Map[K, V]]( name: String )( implicit DK: Diff[K],
                                                                                DV: Diff[V] ): Diff[CC] = {

    new Diff[CC] {

      override def apply( left: CC, right: CC ): Option[Difference] = {
        val DiffMatch( keyDiff, keyMatch ) = DiffMatch.of( left.keys, right.keys )

        val valueDiffs =
          keyMatch.toList.flatMap {
            case ( leftKey, rightKey ) =>
              for {
                leftValue  <- left.get( leftKey )
                rightValue <- right.get( rightKey )
                diff       <- DV( leftValue, rightValue )
              } yield Difference.Keyed( DK.show( leftKey ), diff )
          }

        Ior
          .fromOptions( keyDiff.map( Difference.Set( "key set", _ ) ), NonEmptyList.fromList( valueDiffs ) )
          .map( Difference.Map( name, _ ) )
      }

      override def show( a: CC ): String =
        a.iterator
          .map { case ( k, v ) => DK.show( k ) + " -> " + DV.show( v ) }
          .mkString( "{ ", ", ", " }" )

    }
  }

  def mapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[Map[K, V]] = mapLikeDiff[K, V, Map[K, V]]( "Map" )
  def intMapDiff[V]( implicit DK: Diff[Int], DV: Diff[V] ): Diff[IntMap[V]] =
    mapLikeDiff[Int, V, IntMap[V]]( "IntMap" )
  def longMapDiff[V]( implicit DK: Diff[Long], DV: Diff[V] ): Diff[LongMap[V]] =
    mapLikeDiff[Long, V, LongMap[V]]( "LongMap" )
  def hashMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[HashMap[K, V]] =
    mapLikeDiff[K, V, HashMap[K, V]]( "HashMap" )
  def listMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[ListMap[K, V]] =
    mapLikeDiff[K, V, ListMap[K, V]]( "ListMap" )
  def treeMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[TreeMap[K, V]] =
    mapLikeDiff[K, V, TreeMap[K, V]]( "TreeMap" )

}
