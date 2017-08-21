package net.chwthewke

package object autodiff {
  import scala.collection.{immutable => sci}

  type Seq[A] = sci.Seq[A]
  val Seq: sci.Seq.type = sci.Seq

  type IndexedSeq[A] = sci.IndexedSeq[A]
  val indexedSeq: sci.IndexedSeq.type = sci.IndexedSeq

  type LinearSeq[A] = sci.LinearSeq[A]
  val LinearSeq: sci.LinearSeq.type = sci.LinearSeq

}
