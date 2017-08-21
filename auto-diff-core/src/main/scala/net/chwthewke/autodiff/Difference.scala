package net.chwthewke.autodiff

import cats.Show
import cats.data.NonEmptyList

sealed trait Difference
case class ValueDifference( left: String, right: String )                          extends Difference
case class TaggedDifference( tag: String, difference: Difference )                 extends Difference
case class ObjectDifference( tpe: String, fields: NonEmptyList[TaggedDifference] ) extends Difference

object Difference {
  private def show( difference: Difference ): String = difference match {
    case ValueDifference( l, r )   => s"${util.red( l )} -> ${util.green( r )}"
    case TaggedDifference( n, d )  => s"$n = ${show( d )}"
    case ObjectDifference( n, fs ) => fs.toList.map( show( _ ) ).mkString( s"$n(..., ", ", ", ")" )
  }

  implicit val showDifference: Show[Difference] = show

}
