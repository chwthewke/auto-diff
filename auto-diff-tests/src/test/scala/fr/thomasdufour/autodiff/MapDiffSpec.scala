package fr.thomasdufour.autodiff

import cats.syntax.flatMap._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.cats.implicits._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.collection.immutable.HashMap
import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.LongMap
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.immutable.TreeMap

class MapDiffSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import MapDiffSpec._
  import DiffOps._

  def aMapDiff[K, V, C[v] <: Map[K, v] with Map[K, v]](
      name: String,
      genK: Gen[K],
      genV: Gen[V],
      diff: Diff[C[V]]
  )( implicit F: FromMap[K, C], N: Nudge[V] ): Unit = {

    def mapOfAtLeast( minSize: Int ): Gen[( Map[K, V], Set[K] )] =
      for {
        size   <- Gen.sized( Gen.const ).map( _ max minSize )
        keySet <- setOfN( size, genK )
        values <- Gen.containerOfN[Vector, V]( size, genV )
      } yield ( keySet.toSeq.zip( values ).toMap, keySet )

    val mapsDiffValue: Gen[( C[V], C[V], K, V, V )] = for {
      ( map, keySet ) <- mapOfAtLeast( 1 )
      diffKey         <- Gen.oneOf( keySet.toSeq )
      diffVal         <- N.nudge( genV, map( diffKey ) )
    } yield {
      ( F.fromMap( map ), F.fromMap( map.updated( diffKey, diffVal ) ), diffKey, map( diffKey ), diffVal )
    }

    val mapsDiffKey: Gen[( C[V], C[V], K, K )] = for {
      ( map, keySet ) <- mapOfAtLeast( 2 )
      diffKeys        <- Gen.pick( 2, keySet )
    } yield {
      val k1 :: k2 :: Nil = diffKeys.toList
      (
        F.fromMap( map - k2 ),
        F.fromMap( map - k1 ),
        k1,
        k2
      )
    }

    val mapsExtraKey: Gen[( C[V], C[V], K )] = for {
      ( map, keySet ) <- mapOfAtLeast( 1 )
      extraKey        <- Gen.oneOf( keySet.toSeq )
    } yield ( F.fromMap( map - extraKey ), F.fromMap( map ), extraKey )

    "yield no difference between empty maps" in {
      diff( F.fromMap( Map.empty ), F.fromMap( Map.empty ) ).tree should ===( Z )
    }

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
            M( name, T( T.Set, "key set", U( Some( k1.toString !== k2.toString ), Nil ) ) )
          )
      }
    }

    "yield a difference of keys between maps with an added key" in {
      forAll( mapsExtraKey ) {
        case ( map1, map2, k ) =>
          diff( map1, map2 ).tree should ===(
            M( name, T( T.Set, "key set", U( Some( "" !== k.toString ), Nil ) ) )
          )
      }
    }

    "show the map" in {
      forAll( mapOfAtLeast( 0 ).map( _._1 ).map( F.fromMap ) ) { map =>
        diff.show( map ) should ===(
          map.map { case ( k, v ) => s"$k -> $v" }.mkString( "{ ", ", ", " }" )
        )

      }
    }
  }

  "Diffing immutable Maps" should {
    behave like aMapDiff( "Map", arbitrary[Int], Gen.alphaNumStr, Diff[Map[Int, String]] )
  }

  "Diffing HashMaps" should {
    behave like aMapDiff( "HashMap", arbitrary[Int], Gen.alphaNumStr, Diff[HashMap[Int, String]] )
  }

  "Diffing ListMaps" should {
    behave like aMapDiff( "ListMap", arbitrary[Int], Gen.alphaNumStr, Diff[ListMap[Int, String]] )
  }

  "Diffing TreeMaps" should {
    behave like aMapDiff( "TreeMap", arbitrary[Int], Gen.alphaNumStr, Diff[TreeMap[Int, String]] )
  }

  "Diffing IntMaps" should {
    behave like aMapDiff[Int, String, IntMap]( "IntMap", arbitrary[Int], Gen.alphaNumStr, Diff[IntMap[String]] )
  }

  "Diffing LongMaps" should {
    behave like aMapDiff[Long, String, LongMap]( "LongMap", arbitrary[Long], Gen.alphaNumStr, Diff[LongMap[String]] )
  }

}

object MapDiffSpec {
  def setOfN[A]( n: Int, gen: Gen[A] ): Gen[Set[A]] =
    Set
      .empty[A]
      .tailRecM(
        set =>
          Gen
            .containerOfN[Set, A]( n - set.size, gen )
            .map( set union _ )
            .map(
              newSet =>
                if (newSet.size == n)
                  Right( newSet )
                else
                  Left( newSet )
            )
      )

  sealed trait FromMap[K, C[_]] {
    def fromMap[V]( map: Map[K, V] ): C[V]
  }

  object FromMap {
    implicit def mapFromMap[K]: FromMap[K, Map[K, *]] =
      new FromMap[K, Map[K, *]] {
        override def fromMap[V]( map: Map[K, V] ): Map[K, V] = map
      }

    implicit def hashMapFromMap[K]: FromMap[K, HashMap[K, *]] =
      new FromMap[K, HashMap[K, *]] {
        override def fromMap[V]( map: Map[K, V] ): HashMap[K, V] = HashMap( map.toSeq: _* )
      }

    implicit def listMapFromMap[K]: FromMap[K, ListMap[K, *]] =
      new FromMap[K, ListMap[K, *]] {
        override def fromMap[V]( map: Map[K, V] ): ListMap[K, V] = ListMap( map.toSeq: _* )
      }

    implicit def fromMapSTreeMap[K: Ordering]: FromMap[K, TreeMap[K, *]] =
      new FromMap[K, TreeMap[K, *]] {
        override def fromMap[V]( map: Map[K, V] ): TreeMap[K, V] = TreeMap( map.toSeq: _* )
      }

    implicit val fromMapIntMap: FromMap[Int, IntMap] =
      new FromMap[Int, IntMap] {
        override def fromMap[V]( map: Map[Int, V] ): IntMap[V] = IntMap( map.toSeq: _* )
      }

    implicit val fromMapLongMap: FromMap[Long, LongMap] =
      new FromMap[Long, LongMap] {
        override def fromMap[V]( map: Map[Long, V] ): LongMap[V] = LongMap( map.toSeq: _* )
      }
  }
}
