package fr.thomasdufour.autodiff

import cats.data.Ior
import cats.data.NonEmptyList

sealed trait Difference

object Difference {

  final case class Value( left: String, right: String )                      extends Difference
  final case class Tagged( tag: String, diff: Difference )                   extends Difference
  final case class Unordered( diff: Ior[Value, NonEmptyList[Difference]] )   extends Difference
  final case class Coproduct( name: String, difference: Difference )         extends Difference
  final case class Product( name: String, fields: NonEmptyList[Field] )      extends Difference
  final case class Tuple( name: String, fields: NonEmptyList[Index] )        extends Difference
  final case class Seq( name: String, diffs: NonEmptyList[Index] )           extends Difference
  final case class Set( name: String, diff: Unordered )                      extends Difference
  final case class Map( name: String, diffs: Ior[Set, NonEmptyList[Keyed]] ) extends Difference

  final case class Field( name: String, difference: Difference )
  final case class Keyed( key: String, difference: Difference )
  final case class Index( index: Int, difference: Difference )

}
