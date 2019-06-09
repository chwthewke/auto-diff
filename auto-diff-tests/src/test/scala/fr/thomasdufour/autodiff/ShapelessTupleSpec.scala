package fr.thomasdufour.autodiff

import cats.data.NonEmptyList
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import shapeless.::
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.IsTuple
import shapeless.Nat
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import scala.collection.immutable.TreeSet

object ShapelessTupleSpec {

  trait GenDiff[L] {
    def genSingle: Gen[L]
    def genDiff( diffIndices: Set[Int] ): Gen[( L, L )]
  }

  object GenDiff {
    implicit def genDiffHNil: GenDiff[HNil] =
      new GenDiff[HNil] {
        override def genSingle: Gen[HNil] = HNil

        override def genDiff( diffIndices: Set[Int] ): Gen[( HNil, HNil )] = ( HNil, HNil )
      }

    implicit def genDiffHCons[H, T <: HList]( implicit g: Gen[H], n: Nudge[H], t: GenDiff[T] ): GenDiff[H :: T] =
      new GenDiff[H :: T] {
        override def genSingle: Gen[H :: T] =
          for {
            h <- g
            t <- t.genSingle
          } yield h :: t

        override def genDiff( diffIndices: Set[Int] ): Gen[( H :: T, H :: T )] =
          for {
            h1         <- g
            h2         <- if (diffIndices( 1 )) n.nudge( g, h1 ) else Gen.const( h1 )
            ( t1, t2 ) <- t.genDiff( diffIndices.map( _ - 1 ) )
          } yield ( h1 :: t1, h2 :: t2 )
      }

    implicit def genDiffTuple[T, R]( implicit t: IsTuple[T], g: Generic.Aux[T, R], d: GenDiff[R] ): GenDiff[T] = {
      val _ = t
      new GenDiff[T] {
        override def genSingle: Gen[T] = d.genSingle.map( g.from )

        override def genDiff( diffIndices: Set[Int] ): Gen[( T, T )] = d.genDiff( diffIndices ).map {
          case ( r1, r2 ) => ( g.from( r1 ), g.from( r2 ) )
        }
      }
    }
  }

}

class ShapelessTupleSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks {
  import DiffOps._
  import ShapelessTupleSpec._

  import org.scalacheck.Shrink.shrinkAny

  def genSet( n: Int ): Gen[TreeSet[Int]] = {
    def genSetRec( i: Int, n: Int ): Gen[TreeSet[Int]] =
      if (i > n) TreeSet.empty[Int]
      else
        for {
          s <- genSetRec( i + 1, n )
          b <- arbitrary[Boolean]
        } yield if (b) s + i else s
    genSetRec( 1, n )
  }

  class DiffTupleProp[T <: Product] {
    def check[R <: HList, N <: Nat]( name: String )(
        implicit genDiff: GenDiff[T],
        isTuple: IsTuple[T],
        gen: Generic.Aux[T, R],
        s: Length.Aux[R, N],
        n: ToInt[N],
        diff: Diff[T]
    ): Unit = {
      val _ = ( gen, s, isTuple )

      "validate the diff property" in {
        val tc: Gen[( Set[Int], T, T )] = for {
          s          <- genSet( n.apply() )
          ( t1, t2 ) <- genDiff.genDiff( s )
        } yield ( s, t1, t2 )

        forAll( tc ) {
          case ( diffIndices, t1, t2 ) =>
            def expectedAt( ix: Int ) =
              T.Index( ix, t1.productElement( ix - 1 ).toString !== t2.productElement( ix - 1 ).toString )

            val expectedDiff: DifferenceTree =
              NonEmptyList
                .fromList( diffIndices.toList.map( expectedAt ) )
                .fold[DifferenceTree]( Z )( I.apply( T.Tuple, name, _ ) )

            diff( t1, t2 ).tree should ===( expectedDiff )
        }
      }

      "validate the show property" in {
        forAll( genDiff.genSingle ) { t =>
          diff.show( t ) should ===( t.toString.replaceAll( ",", ", " ) )
        }
      }

    }
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
