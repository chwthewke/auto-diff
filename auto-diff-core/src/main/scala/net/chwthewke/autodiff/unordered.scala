package net.chwthewke.autodiff

import cats.Order
import cats.syntax.option._
import scala.collection.immutable.Map
import scala.collection.immutable.SortedMap
import shapeless.Lazy

object unordered {
  def inAnyOrder[A]( coll: Traversable[A] ): AnyOrder[A] = new AnyOrder[A]( coll )

  class AnyOrder[A]( val coll: Traversable[A] ) extends AnyVal

  def treeAnyOrderDiffShow[A, D <: Difference]( implicit D: Lazy[DiffShow.Aux[A, D]],
                                               O: Order[A] ): DiffShow.Aux[AnyOrder[A], ValueDifference] =
    anyOrderDiffShow[A, SortedMap[A, Vector[A]], D]( SortedMap.empty( O.toOrdering ), _ + _ )

  def hashAnyOrderDiffShow[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[AnyOrder[A], ValueDifference] =
    anyOrderDiffShow[A, Map[A, Vector[A]], D]( Map.empty, _ + _ )

  def equalityAnyOrderDiffShow[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]],
      noHash: custom.NoHash[A] ): DiffShow.Aux[AnyOrder[A], ValueDifference] =
    new AnyOrderDiffShow[A, D]( D ) {
      override def diff( left: AnyOrder[A], right: AnyOrder[A] ): Option[ValueDifference] = {
        val _ = noHash

        val ( removed, added ) =
          right.coll.foldRight[( Vector[A], Vector[A] )]( ( left.coll.toVector, Vector.empty[A] ) ) {
            case ( x, ( r, a ) ) =>
              removeFirst( D.value, x )( r ).fold( ( r, a :+ x ) )( v => ( v, a ) )
          }

        if (removed.isEmpty && added.isEmpty) none
        else ValueDifference( showColl( D.value, removed ), showColl( D.value, added ) ).some
      }
    }

  private abstract class AnyOrderDiffShow[A, D <: Difference]( D: Lazy[DiffShow.Aux[A, D]] )
      extends DiffShow[AnyOrder[A]] {
    override type Out = ValueDifference

    override def show( a: AnyOrder[A] ): String = showColl( D.value, a.coll )
  }

  private def anyOrderDiffShow[A, M <: Map[A, Vector[A]], D <: Difference]( newMap: => M,
                                                                           add: ( M, ( A, Vector[A] ) ) => M )(
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[AnyOrder[A], ValueDifference] =
    new AnyOrderDiffShow[A, D]( D ) {
      override def diff( left: AnyOrder[A], right: AnyOrder[A] ): Option[ValueDifference] = {
        val leftOrd =
          left.coll.foldRight( newMap )( ( x, m ) => add( m, x -> (m.getOrElse( x, Vector() ) :+ x) ) )

        val ( removed, added ) = right.coll.foldRight( ( leftOrd, List.empty[A] ) ) {
          case ( x, ( r, a ) ) =>
            r.get( x ).flatMap( removeFirst( D.value, x ) ).fold( ( r, x :: a ) )( v => ( add( r, x -> v ), a ) )
        }

        ( removed.values.flatten.to[Seq], added ) match {
          case ( Seq(), Seq() ) => none
          case ( r, a )         => ValueDifference( showColl( D.value, r ), showColl( D.value, a ) ).some
        }

      }
    }

  def showColl[A]( D: DiffShow[A], values: Traversable[A] ): String =
    values.map( D.show ).mkString( "{ ", ", ", "... }" )

  def removeFirst[A]( D: DiffShow[A], x: A )( v: Vector[A] ): Option[Vector[A]] =
    v.indexWhere( D.diff( x, _ ).isEmpty ) match {
      case -1 => none
      case ix => v.patch( ix, Vector(), 1 ).some
    }
}
