package fr.thomasdufour.autodiff
package derived

import org.scalatest.Matchers
import org.scalatest.WordSpec
import shapeless.LabelledGeneric
import shapeless.the
import shapeless.tag.@@

class OverridenDerivedDiffSpec extends WordSpec with Matchers {
  import DiffOps._
  import OverridenDerivedDiffSpec._

  "Diffing two equal names" when {
    val left  = Name( "Frederick", "Douglass" )
    val right = Name( "Frederick", "Douglass" )

    "using the default diff" should {
      "return no difference" in {

        defaultNameDiff( left, right ).tree should ===( Z )

      }
    }

    "using the all-case-insensitive diff" should {
      "return no difference" in {

        ciNameDiff( left, right ).tree should ===( Z )

      }
    }

    "using the lastname-case-insensitive diff" should {
      "return no difference" in {

        ciLastNameDiff( left, right ).tree should ===( Z )

      }
    }
  }

  "Diffing two names equal up to case" when {

    val left  = Name( "ROSA", "LUXEMBURG" )
    val right = Name( "Rosa", "Luxemburg" )

    "using the default diff" should {
      "return both differences" in {

        defaultNameDiff( left, right ).tree should ===(
          F( "Name", "first" -> ("ROSA" !== "Rosa"), "last" -> ("LUXEMBURG" !== "Luxemburg") )
        )

      }
    }

    "using the all-case-insensitive diff" should {
      "return no difference" in {

        ciNameDiff( left, right ).tree should ===( Z )

      }
    }

    "using the lastname-case-insensitive diff" should {
      "return the first dfference" in {

        ciLastNameDiff( left, right ).tree should ===( F( "Name", "first" -> ("ROSA" !== "Rosa") ) )

      }
    }

  }

}

object OverridenDerivedDiffSpec {

  case class Name( first: String, last: String )

  val ciStringDiff: Diff[String] = new Diff[String] {
    override def apply( left: String, right: String ): Option[Difference] =
      if (left.toLowerCase == right.toLowerCase) None
      else Some( Difference.Value( show( left ), show( right ) ) )

    override def show( value: String ): String = s"$value *(ci)"
  }

  val defaultNameDiff: Diff[Name] = semi.diff
  val ciNameDiff: Diff[Name] = {
    implicit val ciDiffStrings: Diff[String] = ciStringDiff

    semi.diff
  }

  val ciLastNameDiff: Diff[Name] = {
    implicit val ciDiffLastName = FieldDiff.m( "last" )( Diff[String] )

    semi.diff
  }

  val _ = {
    val genLab = the[LabelledGeneric[Name]]

    implicit val ciDiffLastName = FieldDiff.of[Symbol @@ "last"]( Diff[String] )

    implicitly[HListDiff[genLab.Repr]]
  }

}
