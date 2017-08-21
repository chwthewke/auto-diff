package net.chwthewke.autodiff

import shapeless.labelled.FieldType
import shapeless.ops.hlist.Selector
import shapeless.HList
import shapeless.LabelledGeneric
import shapeless.Witness

import scala.annotation.implicitNotFound

object custom {
  trait NoHash[A]

  object NoHash {
    def apply[A]: NoHash[A] = new NoHash[A] {}
  }

  trait FallbackToEqualAndToString

  // NOTE object name mismatch intended
  object alwaysFallbackToEqualAndToString {
    implicit val instance: FallbackToEqualAndToString = new FallbackToEqualAndToString {}
  }

  class FieldTypeDiffShow[C, K <: Symbol, V, D]( val underlying: DiffShow.Aux[V, D], val witness: Witness.Aux[K] )

  @implicitNotFound( "Could not find a field in ${C} with type ${V} whose name matches the tag in ${K}" )
  trait HasField[C, K <: Symbol, V] {
    type L <: HList
  }

  object HasField {
    private val Instance: HasField[Nothing, Nothing, Nothing] =
      new HasField[Nothing, Nothing, Nothing] {}

    implicit def hasFieldGeneric[C, L <: HList, K <: Symbol, V]( implicit G: LabelledGeneric.Aux[C, L],
                                                                S: Selector[L, FieldType[K, V]] ): HasField[C, K, V] = {
      val _ = ( G, S )
      Instance.asInstanceOf[HasField[C, K, V]]
    }
  }

  def field[C]: FieldTypeDiffShowPartiallyApplied[C] = new FieldTypeDiffShowPartiallyApplied[C]

  class FieldTypeDiffShowPartiallyApplied[C] private[custom] {
    def apply[K <: Symbol, V]( k: Witness.Aux[K], diffShow: DiffShow[V] )(
        implicit F: HasField[C, K, V] ): FieldTypeDiffShow[C, K, V, diffShow.Out] = {
      val _ = F
      new FieldTypeDiffShow[C, K, V, diffShow.Out]( diffShow, k )
    }
  }

  def ignore[A]: DiffShow.Aux[A, Difference] = DiffShow.instance( ( _, _ ) => None, _ => "(ignored)" )

}
