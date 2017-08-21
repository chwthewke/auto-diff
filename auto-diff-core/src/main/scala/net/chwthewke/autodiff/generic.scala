package net.chwthewke.autodiff

import cats.data.NonEmptyList
import cats.syntax.option._
import cats.syntax.profunctor._
import scala.reflect.ClassTag
import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.Coproduct
import shapeless.HList
import shapeless.HNil
import shapeless.Inl
import shapeless.Inr
import shapeless.LabelledGeneric
import shapeless.Lazy
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.tag
import shapeless.tag.@@

object generic {
  val cNilDiffShow: DiffShow.Aux[CNil, ValueDifference] = new DiffShow[CNil] {
    override type Out = ValueDifference

    override def diff( left: CNil, right: CNil ): Option[ValueDifference] = left.impossible

    override def show( a: CNil ): String = a.impossible
  }

  def cconsDiffShow[K <: Symbol, V, T <: Coproduct, DV <: Difference, DT <: Difference](
      implicit K: Witness.Aux[K],
      D: DiffShow.Aux[V, DV],
      T: DiffShow.Aux[T, DT] ): DiffShow.Aux[FieldType[K, V] :+: T, Difference] =
    new DiffShow[FieldType[K, V] :+: T] {

      override type Out = Difference

      override def diff( left: FieldType[K, V] :+: T, right: FieldType[K, V] :+: T ): Option[Difference] =
        ( left, right ) match {
          case ( Inl( l ), Inl( r ) ) => D.diff( l, r )
          case ( Inr( l ), Inr( r ) ) => T.diff( l, r )
          case _                      => ValueDifference( s"${show( left )}", s"${show( right )}" ).some
        }

      override def show( a: FieldType[K, V] :+: T ): String =
        a.eliminate( _ => s"${K.value.name}(...)", T.show )

    }

  def genericCoproductDiffShow[A, K <: Coproduct]( implicit G: LabelledGeneric.Aux[A, K],
                                                  D: Lazy[DiffShow.Aux[K, Difference]] ): DiffShow.Aux[A, Difference] =
    fromLazy( D ).contramap( G.to )

  def hNilDiffShow[C]: DiffShow.Aux[HNil @@ C, NonEmptyList[TaggedDifference]] =
    DiffShow.instance( ( _, _ ) => None, _ => "" )

  def hConsDiffShow[C, K <: Symbol, V, T <: HList, D <: Difference](
      implicit K: Witness.Aux[K],
      H: DiffShow.Aux[V, D],
      D: DiffShow.Aux[T @@ C, NonEmptyList[TaggedDifference]] )
    : DiffShow.Aux[(FieldType[K, V] :: T) @@ C, NonEmptyList[TaggedDifference]] =
    new DiffShow[(FieldType[K, V] :: T) @@ C] {

      override type Out = NonEmptyList[TaggedDifference]

      override def diff( left: (FieldType[K, V] :: T) @@ C,
                        right: (FieldType[K, V] :: T) @@ C ): Option[NonEmptyList[TaggedDifference]] = {

        val headDiff: Option[TaggedDifference] =
          H.diff( left.head, right.head ).map( TaggedDifference( K.value.name, _ ) )
        val tailDiff: List[TaggedDifference] =
          D.diff( tag[C]( left.tail ), tag[C]( right.tail ) ).toList.flatMap( _.toList )

        NonEmptyList
          .fromList( headDiff.toList ++ tailDiff )
      }

      override def show( a: (FieldType[K, V] :: T) @@ C ): String = {
        s"${K.value.name} = ${H.show( a.head )}, ${D.show( tag[C]( a.tail ) )}"
      }
    }

  def diffGenericHList[A, L <: HList](
      implicit G: LabelledGeneric.Aux[A, L],
      T: ClassTag[A],
      D: Lazy[DiffShow.Aux[L @@ A, NonEmptyList[TaggedDifference]]] ): DiffShow.Aux[A, ObjectDifference] = {
    val classTag = util.getClassSimpleName( T.runtimeClass )

    fromLazy( D )
      .dimap( (a: A) => tag[A]( G.to( a ) ) )( ObjectDifference( classTag, _ ) )
      .mapString( s => s"$classTag(${s.dropRight( 2 )})" )
  }

  def fromLazy[A, D]( lzy: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[A, D] =
    DiffShow.instance( ( l, r ) => lzy.value.diff( l, r ), s => lzy.value.show( s ) )

}
