package fr.thomasdufour.autodiff

import cats.Eq
import cats.Show
import cats.data.NonEmptyList
import cats.functor.Profunctor
import cats.syntax.option._
import scala.collection.immutable.Queue
import scala.reflect.ClassTag
import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.Coproduct
import shapeless.HList
import shapeless.HNil
import shapeless.LabelledGeneric
import shapeless.Lazy
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.tag._

abstract class DiffShow[A] {
  type Out

  def diff( left: A, right: A ): Option[Out]

  def show( a: A ): String

  final def contramap[B]( f: B => A ): DiffShow.Aux[B, Out] =
    DiffShow.instance( ( l, r ) => diff( f( l ), f( r ) ), b => show( f( b ) ) )

  final def map[D]( f: Out => D ): DiffShow.Aux[A, D] =
    DiffShow.instance( ( l, r ) => diff( l, r ).map( f ), show )

  final def mapString( f: String => String ): DiffShow.Aux[A, Out] =
    DiffShow.instance( diff, a => f( show( a ) ) )
}

object DiffShow extends DiffShowImplicits6 {
  type Aux[A, D] = DiffShow[A] { type Out = D }

  def apply[A]( implicit ev: DiffShow[A] ): DiffShow.Aux[A, ev.Out] = ev

  implicit val diffShowProfunctor: Profunctor[DiffShow.Aux] = new Profunctor[DiffShow.Aux] {
    override def dimap[A, B, C, D]( fab: DiffShow.Aux[A, B] )( f: C => A )( g: B => D ): DiffShow.Aux[C, D] =
      fab.map( g ).contramap( f )
  }
}

// TODO why delegate and not just mixin?
trait DiffShowImplicits6 extends DiffShowImplicits5 {
  implicit def importDiffShow[A, D]( implicit ex: ExportedDiffShow[A] ): DiffShow.Aux[A, ValueDifference] = ex.self
}

trait DiffShowImplicits5 extends DiffShowImplicits4 {
  implicit val booleanDiffShow: DiffShow.Aux[Boolean, ValueDifference] = {
    import cats.instances.boolean._
    fromEqShow
  }
  implicit val byteDiffShow: DiffShow.Aux[Byte, ValueDifference] = {
    import cats.instances.byte._
    fromEqShow
  }
  implicit val shortDiffShow: DiffShow.Aux[Short, ValueDifference] = {
    import cats.instances.short._
    fromEqShow
  }
  implicit val intDiffShow: DiffShow.Aux[Int, ValueDifference] = {
    import cats.instances.int._
    fromEqShow
  }
  implicit val longDiffShow: DiffShow.Aux[Long, ValueDifference] = {
    import cats.instances.long._
    fromEqShow
  }
  implicit val floatDiffShow: DiffShow.Aux[Float, ValueDifference] = {
    import cats.instances.float._
    fromEqShow
  }
  implicit val doubleDiffShow: DiffShow.Aux[Double, ValueDifference] = {
    import cats.instances.double._
    fromEqShow
  }
  implicit val charDiffShow: DiffShow.Aux[Char, ValueDifference] = {
    import cats.instances.char._
    fromEqShow
  }
  implicit val stringDiffShow: DiffShow.Aux[String, ValueDifference] = {
    import cats.instances.string._
    fromEqShow
  }
  // TODO usual ADTs (option, either, ...)
  // better default for String? other basic types?
}

trait DiffShowImplicits4 extends DiffShowImplicits3 {
  // collections
  // List, Vector are in-order

  implicit def listDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[List[A], ObjectDifference] =
    collections.listDiff

  implicit def queueDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Queue[A], ObjectDifference] =
    collections.queueDiff

  implicit def streamDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Stream[A], ObjectDifference] =
    collections.streamDiff

  implicit def vectorDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Vector[A], ObjectDifference] =
    collections.vectorDiffShow

  implicit def setDiff[A](
      implicit A: Lazy[DiffShow.Aux[unordered.AnyOrder[A], ValueDifference]] ): DiffShow.Aux[Set[A], TaggedDifference] =
    unordered.setDiffShow

  implicit def unorderedCollectionDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[unordered.AnyOrder[A], ValueDifference] =
    unordered.anyOrderDiffShow

  implicit def mapDiff[K, V, DK <: Difference, DV <: Difference](
      implicit K: Lazy[DiffShow.Aux[K, DK]],
      V: Lazy[DiffShow.Aux[V, DV]]
  ): DiffShow.Aux[Map[K, V], ObjectDifference] =
    maps.mapDiffShow

}

trait DiffShowImplicits3 extends DiffShowImplicits2 {
  implicit val cNilDiffShow: DiffShow.Aux[CNil, ValueDifference] = generic.cNilDiffShow

  implicit def cconsDiffShow[K <: Symbol, V, T <: Coproduct, DV <: Difference, DT <: Difference](
      implicit W: Witness.Aux[K],
      D: DiffShow.Aux[V, DV],
      T: DiffShow.Aux[T, DT] ): DiffShow.Aux[FieldType[K, V] :+: T, Difference] = generic.cconsDiffShow

  implicit def genericCoproductDiffShow[A, K <: Coproduct](
      implicit G: LabelledGeneric.Aux[A, K],
      D: Lazy[DiffShow.Aux[K, Difference]] ): DiffShow.Aux[A, Difference] =
    generic.genericCoproductDiffShow

  implicit def hNilDiffShow[C]: DiffShow.Aux[HNil @@ C, NonEmptyList[TaggedDifference]] = generic.hNilDiffShow

  implicit def hConsCustomDiffShow[C, K <: Symbol, V, T <: HList, D <: Difference](
      implicit K: Witness.Aux[K],
      H: custom.FieldTypeDiffShow[C, K, V, D],
      T: DiffShow.Aux[T @@ C, NonEmptyList[TaggedDifference]]
  ): DiffShow.Aux[(FieldType[K, V] :: T) @@ C, NonEmptyList[TaggedDifference]] =
    generic.hConsDiffShow( K, H.underlying, T )

  implicit def diffGenericHList[A, L <: HList](
      implicit G: LabelledGeneric.Aux[A, L],
      T: ClassTag[A],
      D: Lazy[DiffShow.Aux[L @@ A, NonEmptyList[TaggedDifference]]] ): DiffShow.Aux[A, ObjectDifference] =
    generic.diffGenericHList
}

trait DiffShowImplicits2 extends DiffShowImplicits1 {
  implicit def hConsDiffShow[C, K <: Symbol, V, T <: HList, D <: Difference](
      implicit K: Witness.Aux[K],
      H: DiffShow.Aux[V, D],
      D: DiffShow.Aux[T @@ C, NonEmptyList[TaggedDifference]] )
    : DiffShow.Aux[(FieldType[K, V] :: T) @@ C, NonEmptyList[TaggedDifference]] =
    generic.hConsDiffShow

}

trait DiffShowImplicits1 extends DiffShowImplicits0 {
  implicit def eqShowDiffShow[A]( implicit E: Eq[A], S: Show[A] ): DiffShow.Aux[A, ValueDifference] =
    DiffShow.fromEqShow
}

trait DiffShowImplicits0 extends DiffShowFunctions {
  implicit def lastResortDiffShow[A](
      implicit ev: custom.FallbackToEqualAndToString ): DiffShow.Aux[A, ValueDifference] = {
    val _ = ev
    DiffShow.default
  }
}

trait DiffShowFunctions {
  def instance[A, D]( d: ( A, A ) => Option[D], s: A => String ): DiffShow.Aux[A, D] =
    new DiffShow[A] {
      override type Out = D

      override def show( a: A ): String = s( a )

      override def diff( left: A, right: A ): Option[D] = d( left, right )
    }

  def fromEquality[A]( d: ( A, A ) => Boolean, s: A => String ): DiffShow.Aux[A, ValueDifference] =
    instance( ( l, r ) => if (d( l, r )) none else ValueDifference( s( l ), s( r ) ).some, s )

  def fromEqShow[A]( implicit E: Eq[A], S: Show[A] ): DiffShow.Aux[A, ValueDifference] =
    fromEquality( E.eqv, S.show )

  def default[A]: DiffShow.Aux[A, ValueDifference] =
    fromEquality( _ == _, _.toString )

  def fromLazy[A, D]( d: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[A, D] =
    instance( ( l, r ) => d.value.diff( l, r ), a => d.value.show( a ) )

}

class ExportedDiffShow[A]( val self: DiffShow.Aux[A, ValueDifference] ) extends AnyVal
