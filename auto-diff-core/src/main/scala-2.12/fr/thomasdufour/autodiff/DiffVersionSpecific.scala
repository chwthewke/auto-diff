package fr.thomasdufour.autodiff

trait DiffVersionSpecific {
  implicit def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]] =
    IndexedSeqDiff.indexedSeqDiff[A, Vector[A]]( "Array" ).contramap( _.toVector )
}
