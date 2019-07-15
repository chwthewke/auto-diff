package fr.thomasdufour.autodiff
package derived

import cats.syntax.apply._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CoproductDerivationSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import DiffOps._
  import Nudge._
  import CoproductDerivationSpec._

  "A derived Diff for a sealed trait hierarchy" should {

    val diff: Diff[Vehicle] = semi.diff

    "show values according to their constructor" in {
      forAll( genPlane ) { x =>
        diff.show( x ) should ===( s"Plane(name: ${x.name}, wings: ${x.wings}, motors: ${x.motors})" )
      }
    }

    "report a difference between different constructors" in {

      forAll( genCar, genShip ) { ( x, y ) =>
        diff( x, y ).tree should ===( T( T.Coproduct, "Vehicle", "Car(...)" !== "Ship(...)" ) )

      }

    }

    "report differences between different values with the same constructor" in {
      val diffCars = for {
        c          <- genCar
        diffWheels <- Gen.choose( 2, 12 ).except( c.wheels )
      } yield ( c, c.copy( wheels = diffWheels ) )

      forAll( diffCars ) {
        case ( x, y ) =>
          diff( x, y ).tree should ===(
            T( T.Coproduct, "Vehicle", F( "Car", "wheels" -> (x.wheels.toString !== y.wheels.toString) ) )
          )
      }
    }

    "report no difference between identical values" in {
      forAll( genVehicle ) { x =>
        diff( x, x ).tree should ===( Z )
      }
    }

  }

}

object CoproductDerivationSpec {

  sealed trait Vehicle extends Product with Serializable

  final case class Car( name: String, wheels: Int )               extends Vehicle
  final case class Plane( name: String, wings: Int, motors: Int ) extends Vehicle
  final case class Ship( name: String, motorized: Boolean )       extends Vehicle

  val name = for {
    s  <- Gen.sized( Gen.const )
    cs <- Gen.listOfN( s max 1, Gen.alphaLowerChar )
  } yield cs.mkString

  val genCar: Gen[Car]     = ( name, Gen.choose( 2, 12 ) ).mapN( Car )
  val genPlane: Gen[Plane] = ( name, Gen.choose( 1, 3 ).map( _ * 2 ), Gen.choose( 1, 4 ) ).mapN( Plane )
  val genShip: Gen[Ship]   = ( name, arbitrary[Boolean] ).mapN( Ship )

  val genVehicle: Gen[Vehicle] = Gen.oneOf( genCar, genPlane, genShip )

}
