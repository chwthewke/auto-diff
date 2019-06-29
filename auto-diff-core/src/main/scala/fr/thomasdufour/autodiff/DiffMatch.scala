package fr.thomasdufour.autodiff

import cats.Contravariant
import cats.Eq
import cats.Show
import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.option._

case class DiffMatch[A]( difference: Option[Difference.Unordered], matches: List[( A, A )] )

object DiffMatch {

  def of[A]( left: Iterable[A], right: Iterable[A] )( implicit D: Diff[A], H: Hint[A] ): DiffMatch[A] = {

    val ( removed, added, matches, mismatches ) = classify( left, right )

    val unmatchedDifference: Option[Difference.Value] =
      if (removed.isEmpty && added.isEmpty)
        none
      else
        Difference.Value( showUnordered( D, removed ), showUnordered( D, added ) ).some

    val mismatchDifferences: Option[NonEmptyList[Difference]] =
      NonEmptyList.fromList( mismatches )

    val difference = Ior.fromOptions( unmatchedDifference, mismatchDifferences ).map( Difference.Unordered )

    DiffMatch( difference, matches )
  }

  private def classify[A](
      left: Iterable[A],
      right: Iterable[A]
  )( implicit D: Diff[A], H: Hint[A] ): ( List[A], List[A], List[( A, A )], List[Difference] ) = {
    right.foldRight( ( left.toList, List.empty[A], List.empty[( A, A )], List.empty[Difference] ) ) {
      case ( elem, ( remaining, unmatched, matches, mismatches ) ) =>
        def remainingAfter( matched: A ): List[A] = remaining.filterNot( matched == _ )

        def explainMismatch( a: A, d: Difference ): Difference = Difference.Tagged( s"at ${H.show( a )}", d )

        def search( prevMis: Option[( A, Difference )], excessMis: Boolean )(
            cands: List[A]
        ): Either[Option[( A, Difference )], A] = cands match {
          case Nil => Left( prevMis.map { case ( a, d ) => ( a, explainMismatch( a, d ) ) } )
          case cand :: more if H( cand, elem ) =>
            D( cand, elem ) match {
              case None => Right( cand )
              case mis @ Some( _ ) =>
                val ( p, e ) =
                  if (excessMis || prevMis.nonEmpty || H.isDefault) ( none, true )
                  else ( mis.map( cand -> _ ), false )
                search( p, e )( more )
            }
          case _ :: more => search( prevMis, excessMis )( more )
        }

        search( none, false )( remaining ) match {
          case Right( newMatch ) => ( remainingAfter( newMatch ), unmatched, ( newMatch, elem ) :: matches, mismatches )
          case Left( None )      => ( remaining, elem :: unmatched, matches, mismatches )
          case Left( Some( ( mismatched, mismatch ) ) ) =>
            ( remainingAfter( mismatched ), unmatched, matches, mismatch :: mismatches )
        }

    }
  }

  def showUnordered[A]( D: Diff[A], values: Iterable[A] ): String =
    values.map( D.show ).mkString( ", " )

  trait Hint[A] { self =>
    def apply( left: A, right: A ): Boolean
    def isDefault: Boolean = false
    def show( a: A ): String

    def contramap[B]( f: B => A ): Hint[B] =
      new Hint[B] {
        override def apply( left: B, right: B ): Boolean = self.apply( f( left ), f( right ) )
        override def show( b: B ): String                = self.show( f( b ) )

        override def isDefault: Boolean = self.isDefault
      }
  }

  object Hint {
    implicit def defaultMatchHint[A]: Hint[A] = new Hint[A] {
      override def apply( left: A, right: A ): Boolean = true

      override def show( a: A ): String = a.toString // weak

      override def isDefault: Boolean = true
    }

    def byEqShow[A]( implicit E: Eq[A], S: Show[A] ): Hint[A] = new Hint[A] {
      override def apply( left: A, right: A ): Boolean = E.eqv( left, right )

      override def show( a: A ): String = S.show( a )

      override def isDefault: Boolean = false
    }

    def byDiff[A]( implicit D: Diff[A] ): Hint[A] = new Hint[A] {
      override def apply( left: A, right: A ): Boolean = D.apply( left, right ).isEmpty

      override def show( a: A ): String = D.show( a )
    }

    def instance[A]( pred: ( A, A ) => Boolean, showA: A => String ): Hint[A] = new Hint[A] {
      override def apply( left: A, right: A ): Boolean = pred( left, right )

      override def show( a: A ): String = showA( a )

      override def isDefault: Boolean = false
    }

    implicit val HintContravariant: Contravariant[Hint] = new Contravariant[Hint] {
      override def contramap[A, B]( fa: Hint[A] )( f: B => A ): Hint[B] = fa.contramap( f )
    }
  }

}
