package fr.thomasdufour.autodiff

import cats.data.NonEmptySet
import cats.syntax.flatMap._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.AsyncWordSpec
import org.scalatest.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListSet
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

class SetDiffSpec
    extends AsyncWordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  import SetDiffSpec._
  import DiffOps._

  private def setOfAtLeast[A]( minSize: Int )( gen: Gen[A] ): Gen[Set[A]] =
    Gen.sized( Gen.const ).map( _ max minSize ).flatMap( setOfN( _, gen ) )

  private def nonEmptySetOfAtLeast[A: Ordering]( minSize: Int )( gen: Gen[A] ): Gen[NonEmptySet[A]] =
    if (minSize < 1) throw new IllegalArgumentException( "nonEmptySetOfAtLeast expects minSize > 0" )
    else
      setOfAtLeast( minSize )( gen ).map( set => NonEmptySet.fromSetUnsafe( SortedSet.apply( set.toSeq: _* ) ) )

  private def showTheSet[A, B, C](
      name: String,
      gen: Gen[B],
      toColl: B => C,
      toSet: B => Set[A],
      diff: Diff[C]
  ): Unit =
    s"show the $name" in {
      forAll( gen ) { base =>
        val set: C        = toColl( base )
        val expectedParts = toSet( base ).map( _.toString )

        val shown = diff.show( set )

        assert( shown.startsWith( "{ " ) )
        assert( shown.endsWith( " }" ) )

        val parts: Set[String] = shown
          .stripPrefix( "{ " )
          .stripSuffix( " }" )
          .split( ", " )
          .toSet
          .filter( _.nonEmpty ) // note: needs the value strings to be non empty (which we do ensure)

        parts should ===( expectedParts )
      }
    }

  def aSetDiffPossiblyEmpty[A: Ordering, C]( name: String, genA: Gen[A], diff: Diff[C] )(
      implicit F: FromSet[A, C]
  ): Unit = {
    behave like showTheSet(
      name,
      setOfAtLeast( 0 )( genA ),
      F.fromSet,
      (set: Set[A]) => set,
      diff
    )

    "yield no difference between empty sets" in {
      diff( F.fromSet( Set.empty[A] ), F.fromSet( Set.empty[A] ) ).tree should ===( Z )
    }

    "yield a difference between an empty and non-empty set" in {
      forAll( setOfAtLeast( 1 )( genA ) ) { set =>
        diff( F.fromSet( set ), F.fromSet( Set.empty[A] ) ).tree should ===(
          T( T.Set, name, U( Some( U.V( set.toSeq.map( _.toString ): _* )() ), Nil ) )
        )

      }
    }

    aSetDiffCommon( name, genA, diff )
  }

  def aSetDiffNonEmpty[A: Ordering, C]( name: String, genA: Gen[A], diff: Diff[C] )(
      implicit F: FromNonEmptySet[A, C]
  ): Unit = {
    behave like showTheSet(
      name,
      nonEmptySetOfAtLeast( 1 )( genA ),
      F.fromNonEmptySet,
      (nes: NonEmptySet[A]) => nes.toSortedSet,
      diff
    )

    aSetDiffCommon( name, genA, diff )
  }

  def aSetDiffCommon[A: Ordering, C]( name: String, genA: Gen[A], diff: Diff[C] )(
      implicit F: FromNonEmptySet[A, C]
  ): Unit = {
    "yield a difference with a set with extra value" in {
      val setWithExtra: Gen[( C, C, A )] =
        for {
          base  <- nonEmptySetOfAtLeast( 2 )( genA )
          extra <- Gen.oneOf( base.toSortedSet.toSeq )
        } yield (
          F.fromNonEmptySet( NonEmptySet.fromSetUnsafe( base - extra ) ),
          F.fromNonEmptySet( base ),
          extra
        )

      forAll( setWithExtra ) {
        case ( left, right, extra ) =>
          diff( left, right ).tree should ===(
            T( T.Set, name, U( Some( U.V()( extra.toString ) ), Nil ) )
          )
      }
    }

    "yield a difference between different sets" in {
      val setsWithDiffBoth: Gen[( C, C, A, A )] =
        for {
          base     <- nonEmptySetOfAtLeast[A]( 2 )( genA )
          diffVals <- Gen.pick( 2, base.toSortedSet )
        } yield {
          val a1 :: a2 :: _ = diffVals.toList
          (
            F.fromNonEmptySet( NonEmptySet.fromSetUnsafe( base - a1 ) ),
            F.fromNonEmptySet( NonEmptySet.fromSetUnsafe( base - a2 ) ),
            a2,
            a1
          )
        }

      forAll( setsWithDiffBoth ) {
        case ( left, right, leftV, rightV ) =>
          diff( left, right ).tree should ===(
            T( T.Set, name, U( Some( U.V( leftV.toString )( rightV.toString ) ), Nil ) )
          )
      }
    }

  }

  private val alphaNumStr: Gen[String] = {
    Gen.sized( n => Gen.containerOfN[List, Char]( math.sqrt( n + 1d ).toInt, Gen.alphaNumChar ) ).map( _.mkString )
  }

  "diffing immutable Sets" should {
    aSetDiffPossiblyEmpty[String, Set[String]]( "Set", alphaNumStr, Diff.setDiff )
  }

  "diffing immutable SortedSets" should {
    aSetDiffPossiblyEmpty[String, SortedSet[String]]( "SortedSet", alphaNumStr, Diff.sortedSetDiff )
  }

  "diffing ListSets" should {
    aSetDiffPossiblyEmpty[String, ListSet[String]]( "ListSet", alphaNumStr, Diff.listSetDiff )
  }

  "diffing HashSets" should {
    aSetDiffPossiblyEmpty[Long, HashSet[Long]]( "HashSet", arbitrary[Long], Diff.hashSetDiff )
  }

  "diffing TreeSets" should {
    aSetDiffPossiblyEmpty[Int, TreeSet[Int]]( "TreeSet", arbitrary[Int], Diff.treeSetDiff )
  }

  "diffing NonEmptySets" should {
    aSetDiffNonEmpty[String, NonEmptySet[String]]( "NonEmptySet", alphaNumStr, Diff.nesDiff )
  }

}

object SetDiffSpec {
  def setOfN[A]( n: Int, gen: Gen[A] ): Gen[Set[A]] =
    ( n, Set.empty[A] )
      .tailRecM {
        case ( k, acc ) =>
          if (k == 0) Gen.const( Right( acc ) )
          else
            gen.map { a =>
              if (acc( a ))
                Left( ( k, acc ) )
              else
                Left( ( k - 1, acc + a ) )
            }
      }

}
