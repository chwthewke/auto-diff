package fr.thomasdufour.autodiff

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class PrimitivesSpec extends WordSpec with Matchers with TypeCheckedTripleEquals with DiffOps {

  "Diffing ints" when {
    val d = Diff[Int]
    "identical" should {
      "return no difference" in {
        d( 1, 1 ).tree should ===( Z )
      }
    }

    "different" should {
      "return a simple difference" in {
        d( 1, 2 ).tree should ===( "1" !== "2" )
      }
    }
  }

  "Diffing longs" when {
    val d = Diff[Long]
    "identical" should {
      "return no difference" in {
        d( -1L, -1L ).tree should ===( Z )
      }
    }

    "different" should {
      "return a simple difference" in {
        d( 1L, 987654321L ).tree should ===( "1" !== "987654321" )
      }
    }
  }

}
