package fr.thomasdufour

package object autodiff {
  import scala.collection.{immutable => sci}

  private[autodiff] type Seq[A] = sci.Seq[A]
  private[autodiff] val Seq: sci.Seq.type = sci.Seq

  private[autodiff] type IndexedSeq[A] = sci.IndexedSeq[A]
  private[autodiff] val indexedSeq: sci.IndexedSeq.type = sci.IndexedSeq

  private[autodiff] type LinearSeq[A] = sci.LinearSeq[A]
  private[autodiff] val LinearSeq: sci.LinearSeq.type = sci.LinearSeq

  type Diff[A] = DiffShow[A] { type Out <: Difference }
  import language.implicitConversions
  implicit def extendDiffWithApplyMethod[A]( diffInstance: Diff[A] ): Diff.Ops[A] =
    new Diff.Ops[A]( diffInstance )

}
