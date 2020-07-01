package fr.thomasdufour.autodiff

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

  type Hint[A] = fr.thomasdufour.autodiff.Hint[A]
  val Hint: fr.thomasdufour.autodiff.Hint.type = fr.thomasdufour.autodiff.Hint

}
