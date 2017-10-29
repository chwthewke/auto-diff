package net.chwthewke.autodiff

import cats.syntax.option._
import shapeless.Lazy

object matching {
  case class DiffMatch[A]( difference: Option[ValueDifference], matches: Vector[( A, A )] )

  def diffMatch[A, D <: Difference]( left: Traversable[A], right: Traversable[A] )(
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffMatch[A] = {

    val ( removed, added, matches ) =
      right.foldRight( ( left.toVector, Vector.empty[A], Vector.empty[( A, A )] ) ) {
        case ( elem, ( removed, added, matches ) ) =>
          removeFirst( D.value, elem )( removed ).fold( ( removed, added :+ elem, matches ) ) {
            case ( newMatch, v ) => ( v, added, ( newMatch, elem ) +: matches )
          }
      }

    toCollectionDiff( removed, added, matches )( DiffShow.fromLazy( D ) )
  }

  def showUnordered[A]( D: DiffShow[A], values: Traversable[A] ): String =
    values.map( D.show ).mkString( "{ ", ", ", "... }" )

  private def removeFirst[A]( D: DiffShow[A], x: A )( v: Vector[A] ): Option[( A, Vector[A] )] =
    v.indexWhere( D.diff( x, _ ).isEmpty ) match {
      case -1 => none
      case ix => ( v( ix ), v.patch( ix, Vector.empty, 1 ) ).some
    }

  private def toCollectionDiff[A]( removed: Vector[A], added: Vector[A], matches: Vector[( A, A )] )(
      diff: DiffShow[A] ): DiffMatch[A] = {
    val difference =
      if (removed.isEmpty && added.isEmpty)
        none
      else
        ValueDifference( showUnordered( diff, removed ), showUnordered( diff, added ) ).some

    DiffMatch( difference, matches )
  }
}
