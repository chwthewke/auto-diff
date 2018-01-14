package fr.thomasdufour
package autodiff
package generic

import enumeratum.Enum
import enumeratum.EnumEntry
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import extra.enumeratum._

class ExtendedEnumSpec extends WordSpec with Matchers with TypeCheckedTripleEquals with DiffOps {
  import ExtendedEnumSpec._
  import semiauto._

  "Diffing values in a nested Coproduct/Product/Enum" when {
    val D = deriveDiff[ExtendedEnum[Color]]

    import Color.Red

    "identical (Inl)" should {
      "return no difference" in {
        D( EnumValue( Red ), EnumValue( Red ) ).tree should ===( Z )
      }
    }

    "identical (Inr)" should {
      "return no difference" in {
        D( Extension( "Black" ), Extension( "Black" ) ).tree should ===( Z )
      }
    }

    "in different Coproduct cases" should {
      "return a Coproduct case difference" in {
        D( EnumValue( Red ), Extension( "Black" ) ).tree should ===(
          T( T.Coproduct, "ExtendedEnum", V( "EnumValue(...)", "Extension(...)" ) )
        )
      }
    }

  }

}

object ExtendedEnumSpec {
  sealed trait ExtendedEnum[+A <: EnumEntry] {
    def either: Either[String, A]
  }

  final case class EnumValue[+A <: EnumEntry]( value: A ) extends ExtendedEnum[A] {
    override def either: Either[String, A] = Right( value )
  }

  final case class Extension( value: String ) extends ExtendedEnum[Nothing] {
    override def either: Either[String, Nothing] = Left( value )
  }

  sealed trait Color extends EnumEntry
  object Color extends Enum[Color] {
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color

    override def values: IndexedSeq[Color] = findValues
  }
}
