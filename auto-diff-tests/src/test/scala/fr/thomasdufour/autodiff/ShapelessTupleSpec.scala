package fr.thomasdufour.autodiff

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import shapeless.Generic
import shapeless.HList
import shapeless.IsTuple
import shapeless.Nat
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt

class ShapelessTupleSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ShapelessTuples {

  import DiffOps._

  class DiffTupleProp[T <: Product] {
    def check[R <: HList, N <: Nat]( name: String )(
        implicit genDiff: GenDiff[T],
        isTuple: IsTuple[T],
        gen: Generic.Aux[T, R],
        s: Length.Aux[R, N],
        n: ToInt[N],
        diff: Diff[T]
    ): Unit =
      checkDiff[T, R, N, T.Index](
        diff,
        ( ix, v1, v2 ) => T.Index( ix, v1.toString !== v2.toString ),
        ds => I( T.Tuple, name, ds ),
        _.toString.replaceAll( ",", ", " )
      )
  }

  def diffTupleProp[T <: Product] = new DiffTupleProp[T]

  implicit val genInt: Gen[Int] = arbitrary[Int]

  "A Tuple1 diff" should {
    behave like diffTupleProp[Tuple1[Int]].check( "Tuple1" )
  }

  "A Tuple2 diff" should {
    behave like diffTupleProp[( Int, Int )].check( "Tuple2" )
  }

  "A Tuple3 diff" should {
    behave like diffTupleProp[( Int, Int, Int )].check( "Tuple3" )
  }

  "A Tuple4 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int )].check( "Tuple4" )
  }

  "A Tuple5 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int )].check( "Tuple5" )
  }

  "A Tuple6 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int )].check( "Tuple6" )
  }

  "A Tuple7 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int )].check( "Tuple7" )
  }

  "A Tuple8 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple8" )
  }

  "A Tuple9 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple9" )
  }

  "A Tuple10 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple10" )
  }

  "A Tuple11 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple11" )
  }

  "A Tuple12 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple12" )
  }

  "A Tuple13 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )].check( "Tuple13" )
  }

  "A Tuple14 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )]
      .check( "Tuple14" )
  }

  "A Tuple15 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )]
      .check( "Tuple15" )
  }

  "A Tuple16 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )]
      .check( "Tuple16" )
  }

  "A Tuple17 diff" should {
    behave like diffTupleProp[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )]
      .check( "Tuple17" )
  }

  "A Tuple18 diff" should {
    behave like diffTupleProp[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ].check( "Tuple18" )
  }

  "A Tuple19 diff" should {
    behave like diffTupleProp[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ].check( "Tuple19" )
  }

  "A Tuple20 diff" should {
    behave like diffTupleProp[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ].check( "Tuple20" )
  }

  "A Tuple21 diff" should {
    behave like diffTupleProp[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ].check( "Tuple21" )
  }

  "A Tuple22 diff" should {
    behave like diffTupleProp[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ].check( "Tuple22" )
  }

}
