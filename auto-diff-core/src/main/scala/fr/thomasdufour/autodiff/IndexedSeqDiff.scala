package fr.thomasdufour.autodiff

import cats.data.NonEmptyList
import cats.syntax.option._
import scala.annotation.tailrec
import scala.collection.IndexedSeqLike
import scala.collection.mutable

private[autodiff] object IndexedSeqDiff {

  val eos: String = "<end>"

  def at[A, CC <: IndexedSeqLike[A, CC]]( ix: Int, coll: CC ): Option[A] =
    if (coll.isDefinedAt( ix )) coll( ix ).some
    else none

  @tailrec
  def diffIx[A, CC <: IndexedSeqLike[A, CC]]( acc: List[Difference.Index], ix: Int, left: CC, right: CC )(
      implicit D: Diff[A]
  ): List[Difference.Index] =
    ( at[A, CC]( ix, left ), at[A, CC]( ix, right ) ) match {
      case ( None, None ) => acc.reverse
      case ( Some( l ), None ) =>
        diffIx[A, CC]( Difference.Index( ix, Difference.Value( D.show( l ), eos ) ) :: acc, ix + 1, left, right )
      case ( None, Some( r ) ) =>
        diffIx[A, CC]( Difference.Index( ix, Difference.Value( eos, D.show( r ) ) ) :: acc, ix + 1, left, right )
      case ( Some( l ), Some( r ) ) =>
        diffIx[A, CC]( D.apply( l, r ).map( Difference.Index( ix, _ ) ).toList ++ acc, ix + 1, left, right )
    }

  def indexedSeqDiff[A, CC <: IndexedSeqLike[A, CC]]( name: String )( implicit D: Diff[A] ): Diff[CC] =
    new Diff[CC] {
      override def apply( left: CC, right: CC ): Option[Difference] =
        NonEmptyList.fromList( diffIx[A, CC]( Nil, 0, left, right ) ).map( Difference.Seq( name, _ ) )

      override def show( value: CC ): String =
        value.map( D.show ).mkString( s"$name(", ", ", ")" )
    }

  def vectorDiff[A]( implicit D: Diff[A] ): Diff[Vector[A]] = indexedSeqDiff[A, Vector[A]]( "Vector" )
  def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]] =
    indexedSeqDiff[A, mutable.WrappedArray[A]]( "Array" )
      .contramap[Array[A]]( a => a )
}
