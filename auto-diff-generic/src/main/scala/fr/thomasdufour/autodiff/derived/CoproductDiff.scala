package fr.thomasdufour.autodiff
package derived

import cats.syntax.option._
import shapeless.labelled.FieldType
import shapeless.:+:
import shapeless.CNil
import shapeless.Coproduct
import shapeless.Inl
import shapeless.Inr
import shapeless.OrElse
import shapeless.Witness

trait CoproductDiff[C <: Coproduct] {
  def apply( left: C, right: C ): Option[Difference]

  def show( value: C ): String

  def tag( value: C ): String
}

object CoproductDiff {
  implicit val cnilTag: CoproductDiff[CNil] =
    new CoproductDiff[CNil] {
      // $COVERAGE-OFF$
      override def apply( left: CNil, right: CNil ): Option[Difference] = none

      override def show( value: CNil ): String = value.impossible

      override def tag( value: CNil ): String = value.impossible
      // $COVERAGE-ON$
    }

  implicit def cconsTag[K <: Symbol, V, C <: Coproduct](
      implicit K: Witness.Aux[K],
      V: Diff[V] OrElse MkDiff[V],
      T: CoproductDiff[C]
  ): CoproductDiff[FieldType[K, V] :+: C] =
    new CoproductDiff[FieldType[K, V] :+: C] {

      private val diff: Diff[V] = V.unify

      private def showTag( value: FieldType[K, V] :+: C ): String = s"${tag( value )}(...)"

      override def apply( left: FieldType[K, V] :+: C, right: FieldType[K, V] :+: C ): Option[Difference] =
        ( left, right ) match {
          case ( Inl( l ), Inl( r ) ) => diff.apply( l, r )
          case ( Inr( l ), Inr( r ) ) => T( l, r )
          case _                      => Difference.Value( showTag( left ), showTag( right ) ).some
        }

      override def show( value: FieldType[K, V] :+: C ): String =
        value.eliminate( diff.show, T.show )

      override def tag( value: FieldType[K, V] :+: C ): String = value match {
        case Inl( _ ) => K.value.name
        case Inr( x ) => T.tag( x )
      }
    }
}
