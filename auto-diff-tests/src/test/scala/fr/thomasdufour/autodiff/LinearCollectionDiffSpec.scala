package fr.thomasdufour.autodiff

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import com.github.ghik.silencer.silent
import scala.collection.immutable.Queue

class LinearCollectionDiffSpec extends LinearCollectionDiffSpecBase {

  "Diffing NonEmptyChains" should {
    behave like collectionDiffNonEmptyCase( "NonEmptyChain", Diff[NonEmptyChain[Int]] )
  }

  "Diffing NonEmptyLists" should {
    behave like collectionDiffNonEmptyCase( "NonEmptyList", Diff[NonEmptyList[Int]] )
  }

  "Diffing NonEmptyVectors" should {
    behave like collectionDiffNonEmptyCase( "NonEmptyVector", Diff[NonEmptyVector[Int]] )
  }

  "Diffing Chains" should {
    val diff = Diff[Chain[Int]]
    behave like collectionDiffNonEmptyCase( "Chain", diff )
    behave like collectionDiffEmptyCase( "Chain", diff )
  }

  "Diffing Lists" should {
    val diff = Diff[List[Int]]
    behave like collectionDiffNonEmptyCase( "List", diff )
    behave like collectionDiffEmptyCase( "List", diff )
  }

  "Diffing Queues" should {
    val diff = Diff[Queue[Int]]
    behave like collectionDiffNonEmptyCase( "Queue", diff )
    behave like collectionDiffEmptyCase( "Queue", diff )
  }

  "Diffing Streams" should {
    @silent( "deprecated" )
    val diff = Diff[Stream[Int]]
    behave like collectionDiffNonEmptyCase( "Stream", diff )
    behave like collectionDiffEmptyCase( "Stream", diff )
  }

  "Diffing Vectors" should {
    val diff = Diff[Vector[Int]]
    behave like collectionDiffNonEmptyCase( "Vector", diff )
    behave like collectionDiffEmptyCase( "Vector", diff )
  }

  "Diffing Arrays" should {
    val diff = Diff[Array[Int]]
    behave like collectionDiffNonEmptyCase( "Array", diff )
    behave like collectionDiffEmptyCase( "Array", diff )
  }

  "Diffing iterables" should {
    val diff = Diff[Iterable[Int]]
    behave like collectionDiffNonEmptyCase( "an iterable", diff )
    behave like collectionDiffEmptyCase( "an iterable", diff )
  }
}
