package fr.thomasdufour.autodiff

import cats.data.NonEmptyList
import cats.syntax.option._
import scala.annotation.tailrec
import scala.collection.LinearSeqLike
import scala.collection.immutable.Queue

private[autodiff] object LinearSeqDiff {

  val eos: String = "<end>"

  def headTail[A, CC <: LinearSeqLike[A, CC]]( coll: CC ): Option[( A, CC )] =
    if (coll.isEmpty) none
    else ( coll.head, coll.tail ).some

  @tailrec
  def diffIx[A, CC <: LinearSeqLike[A, CC]]( acc: List[Difference.Index], ix: Int, left: CC, right: CC )(
      implicit D: Diff[A] ): List[Difference.Index] = {
    ( headTail[A, CC]( left ), headTail[A, CC]( right ) ) match {
      case ( None, None ) => acc.reverse
      case ( Some( ( lh, lt ) ), Some( ( rh, rt ) ) ) =>
        diffIx[A, CC]( D.apply( lh, rh ).map( Difference.Index( ix, _ ) ).toList ++ acc, ix + 1, lt, rt )
      case ( Some( ( h, t ) ), None ) =>
        diffIx[A, CC]( Difference.Index( ix, Difference.Value( D.show( h ), eos ) ) :: acc, ix + 1, t, right )
      case ( None, Some( ( h, t ) ) ) =>
        diffIx[A, CC]( Difference.Index( ix, Difference.Value( eos, D.show( h ) ) ) :: acc, ix + 1, left, t )
    }
  }

  def diffLinearSeq[A, CC <: LinearSeqLike[A, CC]]( name: String )( implicit D: Diff[A] ): Diff[CC] =
    new Diff[CC] {
      override def apply( left: CC, right: CC ): Option[Difference] =
        NonEmptyList.fromList( diffIx[A, CC]( Nil, 0, left, right ) ).map( Difference.Seq( name, _ ) )

      override def show( value: CC ): String = value.map( D.show ).mkString( s"$name(", ", ", ")" )
    }

  def listDiff[A]( implicit D: Diff[A] ): Diff[List[A]]     = diffLinearSeq[A, List[A]]( "List" )
  def queueDiff[A]( implicit D: Diff[A] ): Diff[Queue[A]]   = diffLinearSeq[A, Queue[A]]( "Queue" )
  def streamDiff[A]( implicit D: Diff[A] ): Diff[Stream[A]] = diffLinearSeq[A, Stream[A]]( "Stream" )
}
