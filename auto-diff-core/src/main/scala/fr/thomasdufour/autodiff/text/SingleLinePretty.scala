package fr.thomasdufour.autodiff
package text

import cats.data.Ior
import cats.data.NonEmptyList
//
import Difference._

case class SingleLinePretty( color: Colorize ) extends Pretty {

  override def show( d: Difference ): String = loop( d )

  private def showValue( v: Value ): String =
    color.left( v.left ) + " -> " + color.right( v.right )

  private def showUnordered( diff: Ior[Value, NonEmptyList[Difference]] ): String =
    "{ " +
      diff
        .bimap( showValue, _.map( loop ) )
        .fold[NonEmptyList[String]]( NonEmptyList.of( _ ), identity, _ :: _ )
        .reduceLeft( _ + ", " + _ ) + " }"

  private def showSeq( name: String, diffs: NonEmptyList[Index] ): String =
    s"$name( " + diffs.map( showIndex ).reduceLeft( _ + ", " + _ ) + " )"

  private def showIndex( ix: Index ): String = s"[${ix.index}] ${loop( ix.difference )}"

  private def showProduct( name: String, fields: NonEmptyList[Field] ): String =
    s"$name( " + fields.map( showField ).reduceLeft( _ + ", " + _ ) + " )"

  private def showField( f: Field ): String = s"${f.name}: ${loop( f.difference )}"

  private def showMap( name: String, diffs: Ior[Set, NonEmptyList[Keyed]] ): String =
    s"$name( " +
      diffs
        .bimap( s => s"${s.name}${showUnordered( s.diff.diff )}", showKeyed )
        .fold( identity, identity, _ + ", " + _ ) + " )"

  private def showKeyed( diffs: NonEmptyList[Keyed] ): String =
    diffs
      .map( k => s"[${k.key}]: ${loop( k.difference )}" )
      .reduceLeft( _ + ", " + _ )

  private def loop( d: Difference ): String = d match {
    case v @ Value( _, _ )          => showValue( v )
    case Tagged( tag, diff )        => s"$tag ${loop( diff )}"
    case Unordered( diff )          => showUnordered( diff )
    case Coproduct( _, difference ) => loop( difference )
    case Product( name, fields )    => showProduct( name, fields )
    case Tuple( _, fields )         => showSeq( "", fields )
    case Seq( name, diffs )         => showSeq( name, diffs )
    case Set( name, diff )          => name + showUnordered( diff.diff )
    case Map( name, diffs )         => showMap( name, diffs )
  }

}
