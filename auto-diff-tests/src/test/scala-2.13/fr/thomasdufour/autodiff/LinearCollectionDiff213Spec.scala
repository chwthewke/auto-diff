package fr.thomasdufour.autodiff

import scala.collection.immutable.ArraySeq

class LinearCollectionDiff213Spec extends LinearCollectionDiffSpecBase {
  "Diffing LazyLists" should {
    val diff = Diff[LazyList[Int]]
    behave like collectionDiffNonEmptyCase( "LazyList", diff )
    behave like collectionDiffEmptyCase( "LazyList", diff )
  }

  "Diffing ArraySeqs" should {
    val diff = Diff[ArraySeq[Int]]
    behave like collectionDiffNonEmptyCase( "ArraySeq", diff )
    behave like collectionDiffEmptyCase( "ArraySeq", diff )
  }
}
