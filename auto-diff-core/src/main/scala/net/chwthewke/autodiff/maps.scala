package net.chwthewke.autodiff

import cats.data.NonEmptyList
import shapeless.Lazy

object maps {

  import matching._

  def mapDiffShow[K, V, DK <: Difference, DV <: Difference](
      implicit DK: Lazy[DiffShow.Aux[K, DK]],
      DV: Lazy[DiffShow.Aux[V, DV]] ): DiffShow.Aux[Map[K, V], ObjectDifference] = {

    val diffKey: DiffShow[K] = DiffShow.fromLazy( DK )

    val diffValue: DiffShow.Aux[V, DV] = DiffShow.fromLazy( DV )

    new DiffShow[Map[K, V]] {
      override type Out = ObjectDifference

      override def diff( left: Map[K, V], right: Map[K, V] ): Option[ObjectDifference] = {
        val DiffMatch( keyDiff, keyMatch ) = matching.diffMatch( left.keys, right.keys )

        val valueDiffs =
          keyMatch.toList.flatMap {
            case ( leftKey, rightKey ) =>
              for {
                leftValue  <- left.get( leftKey )
                rightValue <- right.get( rightKey )
                diff       <- diffValue.diff( leftValue, rightValue )
              } yield TaggedDifference( s"[${diffKey.show( leftKey )}]", diff )
          }

        val taggedKeyDiff = keyDiff.map( TaggedDifference( "keys", _ ) ).toList

        NonEmptyList
          .fromList( taggedKeyDiff ++ valueDiffs )
          .map( ObjectDifference( "Map", _ ) )
      }

      override def show( a: Map[K, V] ): String =
        a.iterator
          .map { case ( k, v ) => diffKey.show( k ) + " -> " + diffValue.show( v ) }
          .mkString( "{ ", ", ", " }" )

    }
  }

}
