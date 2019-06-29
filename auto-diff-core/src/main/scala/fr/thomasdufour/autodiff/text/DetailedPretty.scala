package fr.thomasdufour.autodiff
package text

import cats.data.NonEmptyList
import cats.syntax.reducible._
//
import Difference._

case class DetailedPretty( indentWidth: Int, color: Colorize ) extends Pretty {

  private def indent( line: ( String, Int ) ): ( String, Int ) =
    line match { case ( str, ind ) => ( str, ind + 1 ) }

  private def prettyValues( left: String, right: String ): String =
    color.left( left ) + " -> " + color.right( right )

  private def prettyValueDiff[A]( ind: Int, v: Value ): ( String, Int ) =
    ( prettyValues( v.left, v.right ), ind )

  // TODO make this tailrec some day
  private def prettyIndented( ind: Int, diff: Difference ): NonEmptyList[( String, Int )] = diff match {
    case v @ Value( _, _ ) => NonEmptyList.of( prettyValueDiff( ind, v ) )
    case Tagged( t, d )    => ( t, ind ) :: prettyIndented( ind + 1, d )
    case Coproduct( n, d ) => ( s"in $n", ind ) :: prettyIndented( ind + 1, d )
    case Product( n, fs )  => ( s"in $n", ind ) :: fs.reduceMap( prettyField( ind + 1, _ ) )
    case Tuple( n, ixs )   => ( s"in $n", ind ) :: ixs.reduceMap( prettyIndex( ind + 1, _ ) )
    case Seq( n, ixs )     => ( s"in $n", ind ) :: ixs.reduceMap( prettyIndex( ind + 1, _ ) )
    case Set( n, d )       => ( s"in $n", ind ) :: prettyIndented( ind + 1, d )
    case Unordered( d ) =>
      val contents =
        d.bimap(
            prettyValueDiff( ind + 1, _ ),
            ms => ms.flatMap( prettyIndented( ind + 1, _ ) )
          )
          .fold( NonEmptyList.of( _ ), identity, _ :: _ )
      if (contents.tail.isEmpty)
        NonEmptyList.of( ( "{ " + contents.head._1 + " }", ind ) )
      else
        ( "{", ind ) :: contents ::: NonEmptyList.of( ( "}", ind ) )
    case Map( n, ds ) =>
      ( s"in $n", ind ) ::
        ds.bimap( prettyIndented( ind + 1, _ ), _.reduceMap( prettyKeyed( ind + 1, _ ) ) )
          .fold( identity, identity, _ ::: _ )
  }

  private def prettyField( ind: Int, f: Field ): NonEmptyList[( String, Int )] =
    ( f.name, ind ) :: prettyIndented( ind + 1, f.difference )

  private def prettyIndex( ind: Int, ix: Index ): NonEmptyList[( String, Int )] =
    ( s"[${ix.index}]", ind ) :: prettyIndented( ind + 1, ix.difference )

  private def prettyKeyed[K]( ind: Int, keyed: Keyed ): NonEmptyList[( String, Int )] =
    ( s"at ${keyed.key}", ind ) :: prettyIndented( ind + 1, keyed.difference )

  override def show( d: Difference ): String =
    prettyIndented( 0, d ).reduceMap {
      case ( txt, ind ) =>
        Line( (if (ind == 0) "" else (" " * (indentWidth * (ind - 1)) + "- ")) + txt )
    }.line
}
