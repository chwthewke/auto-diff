package fr.thomasdufour.autodiff

import cats.syntax.option._

// TODO difference: Option[Difference.Value[Vector[A]]] or similar?
case class DiffMatch[A]( difference: Option[Difference], matches: Vector[( A, A )] )

object DiffMatch {

  def of[A]( left: Traversable[A], right: Traversable[A] )( implicit D: Diff[A] ): DiffMatch[A] = {

    val ( removed, added, matches ) =
      right.foldRight( ( left.toVector, Vector.empty[A], Vector.empty[( A, A )] ) ) {
        case ( elem, ( removed, added, matches ) ) =>
          removeFirst( D, elem )( removed ).fold( ( removed, added :+ elem, matches ) ) {
            case ( newMatch, v ) => ( v, added, ( newMatch, elem ) +: matches )
          }
      }

    toCollectionDiff( removed, added, matches )( D )
  }

  def showUnordered[A]( ellipsis: String )( D: Diff[A], values: Traversable[A] ): String =
    values.map( D.show ).mkString( "{ ", ", ", s"$ellipsis }" )

  private def removeFirst[A]( D: Diff[A], x: A )( v: Vector[A] ): Option[( A, Vector[A] )] =
    v.indexWhere( D( x, _ ).isEmpty ) match {
      case -1 => none
      case ix => ( v( ix ), v.patch( ix, Vector.empty, 1 ) ).some
    }

  private def toCollectionDiff[A]( removed: Vector[A], added: Vector[A], matches: Vector[( A, A )] )(
      diff: Diff[A] ): DiffMatch[A] = {
    val difference =
      if (removed.isEmpty && added.isEmpty)
        none
      else
        Difference.Value[Vector[A]]( removed, added, showUnordered( "..." )( diff, _ ) ).some

    DiffMatch( difference, matches )
  }
}
