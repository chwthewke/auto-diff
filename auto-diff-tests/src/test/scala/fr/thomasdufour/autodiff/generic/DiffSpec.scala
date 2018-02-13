package fr.thomasdufour.autodiff
package generic

import org.scalactic.TypeCheckedTripleEquals
//import org.scalatest.Inside._
import org.scalatest.Matchers
import org.scalatest.WordSpec
//import shapeless.syntax.singleton._

class DiffSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import model._

  "Deriving a generic Diff instance" when {
    import generic.auto._

    "type is a simple case class" should {
      "succeed" in {
        Diff[Primitives]
      }
    }

    "type is an ADT" should {
      "succeed" in {
        Diff[Adt]
      }
    }

    "type is an alias" should {
      "succeed" in {
        Diff[Alias]
      }
    }

    "type has collection members" should {
      "succeed" in {
        Diff[Collections]
      }
    }

    "type has common ADT members" should {
      "succeed" in {
        Diff[CommonAdts]
      }
    }

    "type is recursive" should {
      "succeed" in {
        Diff[Recursive]
      }
    }

    "type is an enumeratum enum" should {
      "succeed with the appropriate import" in {
        import extra.enumeratum._

        Diff[AnEnum]
      }
    }

  }
  /*
  "Diffing ints" when {
    val diff = Diff[Int]

    "equal" should {
      "return None" in {

        diff( 1, 1 ) should ===( None )

      }
    }

    "different" should {
      "return the difference" in {
        inside( diff( 1, 2 ) ) {
          case Some( difference ) => difference should ===( "1" !== "2" )
        }
      }
    }
  }

  "Diffing case classes" when {
    val diff = Diff[Primitives]

    "equal" should {
      "return None" in {
        val p = Primitives( 8, 3.14, "foo" )
        val q = Primitives( 8, 3.14, "foo" )

        diff( p, q ) should ===( None )
      }
    }

    "they have a single differing field" should {
      "return the difference" in {
        val p = Primitives( 8, 3.14, "foo" )
        val q = Primitives( 3, 3.14, "foo" )

        inside( diff( p, q ) ) {
          case Some( difference ) =>
            difference should ===(
              obj( "Primitives", tag( "int", "8" !== "3" ) )
            )
        }
      }
    }

    "they have multiple differing fields" should {
      "return (all) the differences" in {
        val p = Primitives( 1, 3.14, "foo" )
        val q = Primitives( 1, 3.16, "bar" )

        inside( diff( p, q ) ) {
          case Some( difference ) =>
            difference should ===(
              obj( "Primitives", tag( "double", "3.14" !== "3.16" ), tag( "string", "foo" !== "bar" ) )
            )
        }
      }
    }

    "customizing diff to ignore a field" when {
      implicit val ignoreStringField = custom.field[Primitives]( 'string.witness, custom.ignore[String] )

      val diff = Diff[Primitives]
      "they differ only in that field" should {
        "return None" in {
          val p = Primitives( 0, 1.0, "foo" )
          val q = Primitives( 0, 1.0, "bar" )

          diff( p, q ) should ===( None )
        }
      }

      "they differ in that field and others" should {
        "return only the other differences" in {
          val p = Primitives( 0, 1.0, "foo" )
          val q = Primitives( 1, 1.0, "bar" )
          inside( diff( p, q ) ) {
            case Some( difference ) =>
              difference should ===(
                obj( "Primitives", tag( "int", "0" !== "1" ) )
              )
          }

        }
      }
    }
  }

  "Diffing ADTs" when {
    val diff = Diff[Adt]

    "equal" should {
      "return None" in {
        val a = Variant1( "foo", 1 )
        val b = Variant1( "foo", 1 )

        diff( a, b ) should ===( None )
      }
    }

    "they are the same variant, but different" should {
      "return the difference" in {
        val a = Variant1( "foo", 1 )
        val b = Variant1( "bar", 1 )

        inside( diff( a, b ) ) {
          case Some( difference ) =>
            difference should ===( obj( "Variant1", tag( "name", "foo" !== "bar" ) ) )
        }
      }
    }

    "they are of different variants" should {
      "report the difference" in {
        val a = Variant1( "foo", 1 )
        val b = Variant2( "foo" :: "bar" :: Nil )

        inside( diff( a, b ) ) {
          case Some( difference ) => difference should ===( "Variant1(...)" !== "Variant2(...)" )
        }
      }
    }

    "they are of different variants, involving an object" should {
      "report the difference" in {
        val a = Variant1( "foo", 1 )
        val b = Variant3

        inside( diff( a, b ) ) {
          case Some( difference ) => difference should ===( "Variant1(...)" !== "Variant3(...)" )
        }
      }
    }
  }

  "diffing collections" when {

    "they are lists of ints" which {
      val diff: Diff[List[Int]] = Diff[List[Int]]

      "are identical" should {
        "report no difference" in {

          val left  = List( 1, 2 )
          val right = List( 1, 2 )

          diff( left, right ) should ===( None )

        }
      }

      "differ in the first position" should {
        "report the difference at index 0" in {

          val left  = List( 1, 3 )
          val right = List( 2, 3 )

          inside( diff( left, right ) ) {
            case Some( difference ) =>
              difference should ===( obj( "List", tag( "[0]", "1" !== "2" ) ) )
          }

        }
      }

      "differ in the second position" should {
        "report the difference at index 1" in {

          val left  = List( 1, 3 )
          val right = List( 1, 2 )

          inside( diff( left, right ) ) {
            case Some( difference ) =>
              difference should ===( obj( "List", tag( "[1]", "3" !== "2" ) ) )
          }

        }
      }

      "differ in several positions" should {
        "report the difference at index 1" in {

          val left  = List( 1, 3 )
          val right = List( 1, 2 )

          inside( diff( left, right ) ) {
            case Some( difference ) =>
              difference should ===( obj( "List", tag( "[1]", "3" !== "2" ) ) )
          }

        }
      }
    }
  }
 */

}
