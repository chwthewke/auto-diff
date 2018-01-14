package fr.thomasdufour.autodiff
package generic

import shapeless.labelled.FieldType
import shapeless.Lazy
import shapeless.HList
import shapeless.HNil
import shapeless.Witness
import shapeless.::

trait HListDiff[L <: HList] {
  def apply( left: L, right: L ): List[Difference.Field]

  def show( value: L ): List[String]
}

object HListDiff extends LowPriorityHListDiffImplicits {

  def apply[L <: HList]( implicit ev: HListDiff[L] ): HListDiff[L] = ev

  implicit val implicitDiffForHNil: HListDiff[HNil] =
    new HListDiff[HNil] {
      override def apply( left: HNil, right: HNil ): List[Difference.Field] = Nil

      override def show( value: HNil ): List[String] = Nil
    }

  implicit def implicitDiffForHCons[K <: Symbol, V, T <: HList]( implicit K: Witness.Aux[K],
                                                                V: Lazy[Diff[V]],
                                                                T: HListDiff[T] ): HListDiff[FieldType[K, V] :: T] =
    hconsDiff( K, V, T )

}

trait LowPriorityHListDiffImplicits {
  implicit def implicitDerivedDiffForHCons[K <: Symbol, V, T <: HList](
      implicit K: Witness.Aux[K],
      V: Lazy[DerivedDiff[V]],
      T: HListDiff[T] ): HListDiff[FieldType[K, V] :: T] =
    hconsDiff( K, V, T )

  def hconsDiff[K <: Symbol, V, T <: HList]( K: Witness.Aux[K],
                                            V: Lazy[Diff[V]],
                                            T: HListDiff[T] ): HListDiff[FieldType[K, V] :: T] =
    new HListDiff[FieldType[K, V] :: T] {
      override def apply( left: FieldType[K, V] :: T, right: FieldType[K, V] :: T ): List[Difference.Field] = {
        V.value( left.head, right.head ).map( Difference.Field( K.value.name, _ ) ).toList ++ T( left.tail, right.tail )
      }

      override def show( value: FieldType[K, V] :: T ): List[String] =
        s"${K.value.name}: ${V.value.show( value.head )}" :: T.show( value.tail )
    }

}
