package fr.thomasdufour.autodiff
package generic

import cats.syntax.option._
import shapeless.labelled.FieldType
import shapeless.:+:
import shapeless.CNil
import shapeless.Coproduct
import shapeless.Inl
import shapeless.Inr
import shapeless.Lazy
import shapeless.Witness

trait CoproductDiff[C <: Coproduct] {
  def apply( left: C, right: C ): Option[Difference]

  def show( value: C ): String
}

object CoproductDiff extends LowPriorityCoproductDiffImplicits {

  implicit val implicitDiffForCNil: CoproductDiff[CNil] =
    new CoproductDiff[CNil] {
      override def apply( left: CNil, right: CNil ): Option[Difference] = none

      override def show( value: CNil ): String = value.impossible
    }

  implicit def implicitDiffForCCons[K <: Symbol, V, T <: Coproduct](
      implicit K: Witness.Aux[K],
      V: Lazy[Diff[V]],
      T: CoproductDiff[T] ): CoproductDiff[FieldType[K, V] :+: T] =
    cconsDiff( K, V, T )

}

trait LowPriorityCoproductDiffImplicits {

  implicit def implicitDerivedDiffForCCons[K <: Symbol, V, T <: Coproduct](
      implicit K: Witness.Aux[K],
      V: Lazy[DerivedDiff[V]],
      T: CoproductDiff[T] ): CoproductDiff[FieldType[K, V] :+: T] =
    cconsDiff( K, V, T )

  def cconsDiff[K <: Symbol, V, T <: Coproduct]( K: Witness.Aux[K],
                                                V: Lazy[Diff[V]],
                                                T: CoproductDiff[T] ): CoproductDiff[FieldType[K, V] :+: T] =
    new CoproductDiff[:+:[FieldType[K, V], T]] {
      override def apply( left: :+:[FieldType[K, V], T], right: :+:[FieldType[K, V], T] ): Option[Difference] =
        ( left, right ) match {
          case ( Inl( l ), Inl( r ) ) => V.value( l, r )
          case ( Inr( l ), Inr( r ) ) => T( l, r )
          case _                      => Difference.Value( left, right, show ).some
        }

      override def show( value: :+:[FieldType[K, V], T] ): String =
        value.eliminate( _ => s"${K.value.name}(...)", T.show )
    }

}
