package fr.thomasdufour.autodiff
package generic

import cats.data.NonEmptyList
import shapeless.Coproduct
import shapeless.HList
import shapeless.LabelledGeneric
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

@implicitNotFound( "Could not generically derive Diff for ${A}" )
abstract class DerivedDiff[A] extends Diff[A] { self =>

  def apply( left: A, right: A ): Option[Difference]

  def show( value: A ): String

}

object DerivedDiff {
  implicit def genericProductDiff[A, L <: HList](
      implicit G: LabelledGeneric.Aux[A, L],
      D: HListDiff[L],
      C: ClassTag[A]
  ): DerivedDiff[A] =
    new DerivedDiff[A] {
      override def apply( left: A, right: A ): Option[Difference] =
        NonEmptyList
          .fromList( D( G.to( left ), G.to( right ) ) )
          .map( Difference.Product( getClassSimpleName( C.runtimeClass ), _ ) )

      override def show( value: A ): String =
        D.show( G.to( value ) ).mkString( s"${getClassSimpleName( C.runtimeClass )}(", ", ", ")" )
    }

  implicit def genericCoproductDiff[A, C <: Coproduct](
      implicit G: LabelledGeneric.Aux[A, C],
      D: CoproductDiff[C],
      C: ClassTag[A]
  ): DerivedDiff[A] =
    new DerivedDiff[A] {
      override def apply( left: A, right: A ): Option[Difference] =
        D( G.to( left ), G.to( right ) ).map( Difference.Coproduct( getClassSimpleName( C.runtimeClass ), _ ) )

      override def show( value: A ): String = D.show( G.to( value ) )
    }

}
