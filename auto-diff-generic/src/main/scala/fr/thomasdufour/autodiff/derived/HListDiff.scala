package fr.thomasdufour.autodiff
package derived

import shapeless.labelled.FieldType
import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.OrElse
import shapeless.Witness

trait HListDiff[L <: HList] {
  def apply( left: L, right: L ): List[Difference.Field]

  def show( value: L ): List[String]
}

object HListDiff {
  def apply[L <: HList]( implicit ev: HListDiff[L] ): HListDiff[L] = ev

  implicit val implicitDiffForHNil: HListDiff[HNil] =
    new HListDiff[HNil] {
      override def apply( left: HNil, right: HNil ): List[Difference.Field] = Nil

      override def show( value: HNil ): List[String] = Nil
    }

  implicit def hconsDiff[K <: Symbol, V, T <: HList](
      implicit K: Witness.Aux[K],
      V: FieldDiff[K, V] OrElse (Diff[V] OrElse MkDiff[V]),
      T: HListDiff[T]
  ): HListDiff[FieldType[K, V] :: T] =
    new HListDiff[FieldType[K, V] :: T] {
      private val diff: Diff[V] = V.fold( _.diff, _.unify )

      override def apply( left: FieldType[K, V] :: T, right: FieldType[K, V] :: T ): List[Difference.Field] = {
        diff.apply( left.head, right.head ).map( Difference.Field( K.value.name, _ ) ).toList ++
          T( left.tail, right.tail )
      }

      override def show( value: FieldType[K, V] :: T ): List[String] =
        s"${K.value.name}: ${diff.show( value.head )}" :: T.show( value.tail )
    }

}
