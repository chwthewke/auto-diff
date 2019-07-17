package fr.thomasdufour.autodiff

import cats.data.NonEmptyVector
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class LinearCollectionDiffSpecBase
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ScalaCheckConfig {

  import AsIterable.AsIterableOps
  import DiffOps._
  import LinearCollectionDiffSpecBase._

  def collectionDiffNonEmptyCase[C[_]: AsIterable]( name: String, diff: Diff[C[Int]] )(
      implicit F: FromNonEmptyVector[Int, C]
  ): Unit = {
    s"yield no diff between identical ${name}s" in {
      forAll( genColl1[C, Int]( arbitrary[Int] ) ) { ints =>
        diff( ints, ints ).tree should ===( Z )
      }
    }

    s"yield a diff with a single index differing between ${name}s" in {
      forAll( genIxDiffColls[C, Int]( arbitrary[Int] ) ) {
        case ( ix, xs, ys ) =>
          diff( xs, ys ).tree should ===( I( T.Seq, name, ix -> (xs.at( ix ).toString !== ys.at( ix ).toString) ) )
      }
    }

    s"yield a diff with a single element added to a $name" in {
      forAll( genColl1[C, Int]( arbitrary[Int] ), arbitrary[Int] ) {
        case ( ints, x ) =>
          val intsApp = F.fromNev( NonEmptyVector.fromVectorUnsafe( ints.asIterable.toVector :+ x ) )

          diff( ints, intsApp ).tree should ===( I( T.Seq, name, ints.size -> ("<end>" !== x.toString) ) )
      }
    }

    s"show the elements in a tagged comma-separated list" in {
      forAll( genColl1[C, Int]( arbitrary[Int] ) ) { ints =>
        diff.show( ints ) should ===( ints.asIterable.mkString( s"$name(", ", ", ")" ) )
      }
    }
  }

  def collectionDiffEmptyCase[C[_]: AsIterable]( name: String, diff: Diff[C[Int]] )(
      implicit F: FromVector[Int, C]
  ): Unit = {

    s"yield no diff between empty ${name}s" in {

      diff( F.fromVector( Vector.empty ), F.fromVector( Vector.empty ) ).tree should ===( Z )
    }

    s"yield a diff between empty and singleton ${name}s" in {
      forAll( arbitrary[Int] ) { x =>
        diff( F.fromVector( Vector( x ) ), F.fromVector( Vector.empty[Int] ) ).tree should ===(
          I( T.Seq, name, 0 -> (x.toString !== "<end>") )
        )
      }

    }

  }

}

object LinearCollectionDiffSpecBase {

  def genColl[C[_], A]( elem: Gen[A] )( implicit F: FromVector[A, C] ): Gen[C[A]] =
    Gen.sized( Gen.const ).flatMap( Gen.containerOfN[Vector, A]( _, elem ) ).map( F.fromVector )

  def genColl1[C[_], A]( elem: Gen[A] )( implicit F: FromNonEmptyVector[A, C] ) =
    Gen
      .sized( Gen.const )
      .flatMap( n => Gen.containerOfN[Vector, A]( n max 1, elem ) )
      .map( NonEmptyVector.fromVectorUnsafe )
      .map( F.fromNev )

  def genIxDiffColls[C[_], A](
      elem: Gen[A]
  )( implicit F: FromNonEmptyVector[A, C], N: Nudge[A] ): Gen[( Int, C[A], C[A] )] =
    for {
      size <- Gen.sized( Gen.const ).map( _ max 1 )
      ix   <- Gen.choose( 0, size - 1 )
      xs   <- Gen.containerOfN[Vector, A]( size, elem )
      y    <- N.nudge( elem, xs( ix ) )
    } yield (
      ix,
      F.fromNev( NonEmptyVector.fromVectorUnsafe( xs ) ),
      F.fromNev( NonEmptyVector.fromVectorUnsafe( xs.updated( ix, y ) ) )
    )

}
