package fr.thomasdufour.autodiff
package derived

import cats.data.NonEmptyList
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import shapeless.Coproduct
import shapeless.HList
import shapeless.LabelledGeneric
import shapeless.Lazy

@implicitNotFound( "Could not derive an instance of Diff[${A}]" )
trait MkDiff[A] extends Diff[A]

object MkDiff extends MkDiffDerivation {
  def apply[A]( implicit mda: MkDiff[A] ): MkDiff[A] = mda
}

private[derived] abstract class MkDiffDerivation {

  implicit def mkDiffGenericCoproduct[T, R <: Coproduct](
      implicit gen: LabelledGeneric.Aux[T, R],
      diffR: Lazy[CoproductDiff[R]],
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
