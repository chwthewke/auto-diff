package fr.thomasdufour.autodiff
package derived

import enumeratum.Enum
import enumeratum.EnumEntry
import java.time.OffsetDateTime
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class DerivationSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import DerivationSpec._

  "Deriving a generic Diff instance" when {
    import semi.diff

    "type is a simple case class" should {
      "succeed" in {
        diff[Primitives]
      }
    }

    "type is an ADT" should {
      "succeed" in {
        diff[Adt]
      }
    }

    "type is an alias" should {
      "succeed" in {
        Diff[Alias]
      }
    }

    "type has collection members" should {
      "succeed" in {
        diff[Collections]
      }
    }

    "type has common ADT members" should {
      "succeed" in {
        diff[CommonAdts]
      }
    }

    "type is recursive" should {
      "succeed" in {
        diff[Recursive]
      }
    }

    "type is an enumeratum enum" should {
      "succeed with the appropriate import" in {
        import extra.enumeratum._

        Diff[AnEnum]
      }
    }
  }
}

object DerivationSpec {
  type Alias = Int

  case class Primitives( int: Int, double: Double, string: String )

  case class Collections( vector: Vector[Boolean], list: List[Int], set: Set[String], map: Map[Long, String] )

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
