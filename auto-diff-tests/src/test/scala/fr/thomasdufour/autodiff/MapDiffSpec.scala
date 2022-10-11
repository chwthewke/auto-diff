package fr.thomasdufour.autodiff

import cats.data.NonEmptyMap
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.immutable.HashMap
import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.LongMap
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap

class MapDiffSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import MapDiffSpec._
  import DiffOps._
  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  private def mapOfAtLeast[K, V](
      minSize: Int
  )( genK: Gen[K], genV: Gen[V] ): Gen[( Map[K, V], Set[K] )] =
    for {
      size   <- Gen.sized( Gen.const ).map( _ max minSize )
      keySet <- setOfN( size, genK )
      values <- Gen.containerOfN[Vector, V]( size, genV )
    } yield {
      val map = keySet.toSeq.zip( values ).toMap
      ( map, map.keySet )
    }

  private def nonEmptyMapOfAtLeast[K: Ordering, V](
      minSize: Int
  )( genK: Gen[K], genV: Gen[V] ): Gen[( NonEmptyMap[K, V], Set[K] )] =
    if (minSize < 1)
      throw new IllegalArgumentException( "nonEmptyMapOfAtLeast expects minSize > 0" )
    else
      mapOfAtLeast( minSize )( genK, genV )
        .map { case ( map, keys ) => ( NonEmptyMap.fromMapUnsafe( SortedMap.apply( map.toSeq: _* ) ), keys ) } // weird SortedMap ctor bc 2.12 xcompat

  def aMapDiffPossiblyEmpty[K, V, C <: Map[K, V]](
      name: String,
      genK: Gen[K],
      genV: Gen[V],
      diff: Diff[C]
  )( implicit K: Ordering[K], F: FromMap[K, V, C], N: Nudge[V] ): Unit = {

    "yield no difference between empty maps" in {
      diff( F.fromMap( Map.empty ), F.fromMap( Map.empty ) ).tree should ===( Z )
    }

    s"show the $name" in {
      forAll( mapOfAtLeast( 0 )( genK, genV ).map( _._1 ).map( F.fromMap ) ) { map =>
        diff.show( map ) should ===(
          map.map { case ( k, v ) => s"$k -> $v" }.mkString( "{ ", ", ", " }" )
        )
      }
    }

    "yield a difference between an empty and non empty map" in {
      forAll( mapOfAtLeast( 1 )( genK, genV ).map {
        case ( m, k ) => ( F.fromMap( m ), F.fromMap( Map.empty[K, V] ), k )
      } ) {
        case ( map, empty, keys ) =>
          diff( map, empty ).tree should ===(
            M( name, T( T.Set, "key set", U( Some( U.V( keys.map( _.toString ), Set.empty ) ), Nil ) ) )
          )
      }
    }

    aMapDiffCommon[K, V, C]( name, genK, genV, diff )

  }

  def aMapDiffNonEmpty[K: Ordering, V, C]( name: String, genK: Gen[K], genV: Gen[V], diff: Diff[C] )(
      implicit F: FromNonEmptyMap[K, V, C],
      N: Nudge[V]
  ): Unit = {

    s"show the $name" in {
      forAll( nonEmptyMapOfAtLeast( 1 )( genK, genV ).map( _._1 ) ) { map =>
        diff.show( F.fromNonEmptyMap( map ) ) should ===(
          map.toSortedMap.map { case ( k, v ) => s"$k -> $v" }.mkString( "{ ", ", ", " }" )
        )
      }
    }

    aMapDiffCommon[K, V, C]( name, genK, genV, diff )

  }

  def aMapDiffCommon[K: Ordering, V, C](
      name: String,
      genK: Gen[K],
      genV: Gen[V],
      diff: Diff[C]
  )( implicit F: FromNonEmptyMap[K, V, C], N: Nudge[V] ): Unit = {

    val mapsDiffValue: Gen[( C, C, K, V, V )] = for {
      ( map, keySet ) <- nonEmptyMapOfAtLeast( 1 )( genK, genV )
      diffKey         <- Gen.oneOf( keySet.toSeq )
      origVal = map( diffKey ).get
      diffVal <- N.nudge( genV, origVal )
    } yield {
      (
        F.fromNonEmptyMap( map ),
        F.fromNonEmptyMap( map.updateWith( diffKey )( _ => diffVal ) ),
        diffKey,
        origVal,
        diffVal
      )
    }

    val mapsDiffKey: Gen[( C, C, K, K )] = for {
      ( map, keySet ) <- nonEmptyMapOfAtLeast( 2 )( genK, genV )
      diffKeys        <- Gen.pick( 2, keySet )
    } yield {
      val k1 :: k2 :: Nil = diffKeys.toList
      (
        F.fromNonEmptyMap( NonEmptyMap.fromMapUnsafe( map - k2 ) ),
        F.fromNonEmptyMap( NonEmptyMap.fromMapUnsafe( map - k1 ) ),
        k1,
        k2
      )
    }

    val mapsExtraKey: Gen[( C, C, K )] = for {
      ( map, keySet ) <- nonEmptyMapOfAtLeast( 2 )( genK, genV )
      extraKey        <- Gen.oneOf( keySet.toSeq )
    } yield ( F.fromNonEmptyMap( NonEmptyMap.fromMapUnsafe( map - extraKey ) ), F.fromNonEmptyMap( map ), extraKey )

    "yield a difference of values between maps with the same keys" in {
      forAll( mapsDiffValue ) {
        case ( map1, map2, k, v1, v2 ) =>
          diff( map1, map2 ).tree should ===( M( name, k.toString -> (v1.toString !== v2.toString) ) )
      }
    }

    "yield a difference of keys between maps with the different keys" in {
      forAll( mapsDiffKey ) {
        case ( map1, map2, k1, k2 ) =>
          diff( map1, map2 ).tree should ===(
            M( name, T( T.Set, "key set", U( Some( U.V( k1.toString )( k2.toString ) ), Nil ) ) )
          )
      }
    }

    "yield a difference of keys between maps with an added key" in {
      forAll( mapsExtraKey ) {
        case ( map1, map2, k ) =>
          diff( map1, map2 ).tree should ===(
            M( name, T( T.Set, "key set", U( Some( U.V()( k.toString ) ), Nil ) ) )
          )
      }
    }

  }

  "Diffing immutable Maps" should {
    behave like aMapDiffPossiblyEmpty( "Map", arbitrary[Int], Gen.alphaNumStr, Diff[Map[Int, String]] )
  }

  "Diffing immutable SortedMaps" should {
    behave like aMapDiffPossiblyEmpty( "SortedMap", arbitrary[Int], Gen.alphaNumStr, Diff[SortedMap[Int, String]] )
  }

  "Diffing HashMaps" should {
    behave like aMapDiffPossiblyEmpty( "HashMap", arbitrary[Int], Gen.alphaNumStr, Diff[HashMap[Int, String]] )
  }

  "Diffing ListMaps" should {
    behave like aMapDiffPossiblyEmpty( "ListMap", arbitrary[Int], Gen.alphaNumStr, Diff[ListMap[Int, String]] )
  }

  "Diffing TreeMaps" should {
    behave like aMapDiffPossiblyEmpty( "TreeMap", arbitrary[Int], Gen.alphaNumStr, Diff[TreeMap[Int, String]] )
  }

  "Diffing IntMaps" should {
    behave like aMapDiffPossiblyEmpty( "IntMap", arbitrary[Int], Gen.alphaNumStr, Diff[IntMap[String]] )
  }

  "Diffing LongMaps" should {
    behave like aMapDiffPossiblyEmpty( "LongMap", arbitrary[Long], Gen.alphaNumStr, Diff[LongMap[String]] )
  }

  "Diffing NonEmptyMaps" should {
    behave like aMapDiffNonEmpty( "NonEmptyMap", arbitrary[Int], Gen.alphaNumStr, Diff[NonEmptyMap[Int, String]] )
  }
}

object MapDiffSpec {
  def setOfN[A]( n: Int, gen: Gen[A] ): Gen[Set[A]] =
    SetDiffSpec.setOfN( n, gen )
}
