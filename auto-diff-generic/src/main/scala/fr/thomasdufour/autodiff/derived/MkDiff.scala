package fr.thomasdufour.autodiff
package derived

import cats.data.NonEmptyList
import cats.syntax.option._
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless.labelled.FieldType
import shapeless.:+:
import shapeless.CNil
import shapeless.Coproduct
import shapeless.HList
import shapeless.Inl
import shapeless.Inr
import shapeless.LabelledGeneric
import shapeless.Lazy
import shapeless.OrElse
import shapeless.Witness

@implicitNotFound( "Could not derive an instance of Diff[${A}]" )
trait MkDiff[A] extends Diff[A]

object MkDiff extends MkDiffDerivation {
  def apply[A]( implicit mda: MkDiff[A] ): MkDiff[A] = mda
}

private[derived] abstract class MkDiffDerivation {
  implicit val mkDiffCnil: MkDiff[CNil] =
    new MkDiff[CNil] {
      // $COVERAGE-OFF$
      override def apply( left: CNil, right: CNil ): Option[Difference] = none

      override def show( value: CNil ): String = value.impossible
      // $COVERAGE-ON$
    }

  implicit def mkDiffCcons[K <: Symbol, V, T <: Coproduct](
      implicit K: Witness.Aux[K],
      V: Diff[V] OrElse MkDiff[V],
      T: MkDiff[T]
  ): MkDiff[FieldType[K, V] :+: T] =
    new MkDiff[FieldType[K, V] :+: T] {
      override def apply( left: FieldType[K, V] :+: T, right: FieldType[K, V] :+: T ): Option[Difference] =
        ( left, right ) match {
          case ( Inl( l ), Inl( r ) ) => V.unify.apply( l, r )
          case ( Inr( l ), Inr( r ) ) => T( l, r )
          case _                      => Difference.Value( show( left ), show( right ) ).some
        }

      override def show( value: FieldType[K, V] :+: T ): String =
        value.eliminate( _ => s"${K.value.name}(...)", T.show )
    }

  implicit def mkDiffGenericCoproduct[T, R <: Coproduct](
      implicit gen: LabelledGeneric.Aux[T, R],
      diffR: Lazy[MkDiff[R]],
      tag: ClassTag[T]
  ): MkDiff[T] =
    new MkDiff[T] {
      override def apply( left: T, right: T ): Option[Difference] =
        diffR.value
          .apply( gen.to( left ), gen.to( right ) )
          .map( Difference.Coproduct( getClassSimpleName( tag.runtimeClass ), _ ) )

      override def show( value: T ): String = diffR.value.show( gen.to( value ) )
    }

  implicit def mkDiffGenericHList[T, R <: HList](
      implicit gen: LabelledGeneric.Aux[T, R],
      diffR: Lazy[HListDiff[R]],
      tag: ClassTag[T]
  ): MkDiff[T] =
    new MkDiff[T] {
      override def apply( left: T, right: T ): Option[Difference] =
        NonEmptyList
          .fromList( diffR.value.apply( gen.to( left ), gen.to( right ) ) )
          .map( Difference.Product( getClassSimpleName( tag.runtimeClass ), _ ) )

      override def show( value: T ): String =
        diffR.value.show( gen.to( value ) ).mkString( s"${getClassSimpleName( tag.runtimeClass )}(", ", ", ")" )

    }
}
