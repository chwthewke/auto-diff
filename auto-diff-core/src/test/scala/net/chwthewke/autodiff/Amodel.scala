package net.chwthewke.autodiff

import enumeratum.Enum
import enumeratum.EnumEntry
import java.time.OffsetDateTime

object model {

  type Alias = Int

  case class Primitives( int: Int, double: Double, string: String )

  case class Collections( vector: Vector[Boolean], list: List[Int], set: Set[String] )

  sealed trait Adt
  case class Variant1( name: String, value: Int ) extends Adt
  case class Variant2( values: List[String] )     extends Adt
  case object Variant3                            extends Adt

  sealed trait AnEnum extends EnumEntry
  object AnEnum extends Enum[AnEnum] {
    case object Value1 extends AnEnum
    case object Value2 extends AnEnum
    case object Value3 extends AnEnum

    override def values: IndexedSeq[AnEnum] = findValues
  }

  case class CommonAdts( option: Option[Int], either: Either[String, Int] )

  case class Recursive( name: String, children: List[Recursive] )

  case class WithTime( value: String, date: OffsetDateTime )
}
