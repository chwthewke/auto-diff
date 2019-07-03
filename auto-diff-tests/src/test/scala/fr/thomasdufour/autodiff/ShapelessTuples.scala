package fr.thomasdufour.autodiff

import cats.data.NonEmptyList
import cats.syntax.flatMap._
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalatest.Matchers
import org.scalatest.WordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.immutable.TreeSet
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.::
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.IsTuple
import shapeless.Nat

trait ShapelessTuples extends ScalaCheckConfig { self: WordSpecLike with Matchers with ScalaCheckDrivenPropertyChecks =>

  import DiffOps._
  import org.scalacheck.Shrink.shrinkAny

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

  def genSet( n: Int ): Gen[TreeSet[Int]] =
    ( ( n, TreeSet.empty[Int] ) ).tailRecM {
      case ( k, acc ) =>
        if (k == 0)
          Gen.const( Right( acc ) )
        else
          Gen
            .oneOf( acc, acc + k )
            .map( s => Left( ( k - 1, s ) ) )
    }

  def checkDiff[T <: Product, R <: HList, N <: Nat, D](
      diff: Diff[T],
      expectedItemDiff: ( Int, Any, Any ) => D,
      mkExpectedDiff: NonEmptyList[D] => DifferenceTree,
      expectedShow: T => String
  )(
      implicit genDiff: GenDiff[T],
      isTuple: IsTuple[T],
      gen: Generic.Aux[T, R],
      s: Length.Aux[R, N],
      n: ToInt[N]
  ): Unit = {
    val _ = ( gen, s, isTuple )

    "validate the diff property" in {
      val tc: Gen[( Set[Int], T, T )] = for {
        s          <- genSet( n.apply() )
        ( t1, t2 ) <- genDiff.genDiff( s )
      } yield ( s, t1, t2 )

      forAll( tc ) {
        case ( diffIndices, t1, t2 ) =>
          def expectedAt( ix: Int ): D =
            expectedItemDiff( ix, t1.productElement( ix - 1 ), t2.productElement( ix - 1 ) )

          val expectedDiff: DifferenceTree =
            NonEmptyList
              .fromList( diffIndices.toList.map( expectedAt ) )
              .fold[DifferenceTree]( Z )( mkExpectedDiff )

          diff( t1, t2 ).tree should ===( expectedDiff )
      }
    }

    "validate the show property" in {
      forAll( genDiff.genSingle ) { t =>
        diff.show( t ) should ===( expectedShow( t ) )
      }
    }

  }

}
