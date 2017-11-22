package fr.thomasdufour.autodiff

import cats.data.NonEmptyList
import cats.syntax.option._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import shapeless.Lazy

object collections {
  abstract class LinearSeqDiffShow[A, L <: LinearSeq[A], D <: Difference]( D: Lazy[DiffShow.Aux[A, D]] )
      extends DiffShow[L] {

    override type Out = ObjectDifference

    @tailrec
    private def diffIx( ix: Int, left: scala.Seq[A], right: scala.Seq[A] ): Option[ObjectDifference] = {
      def tagged( d: Difference ): TaggedDifference  = TaggedDifference( s"[$ix]", d )
      def object_( d: Difference ): ObjectDifference = ObjectDifference( name, NonEmptyList.of( tagged( d ) ) )
      def eos: String                                = s"<end of $name>"

      ( left, right ) match {
        case ( LinearSeq( headLeft, tailLeft @ _* ), LinearSeq( headRight, tailRight @ _* ) ) =>
          val headDiff = D.value.diff( headLeft, headRight )
          if (headDiff.isDefined)
            headDiff.map( d => object_( d ) )
          else
            diffIx( ix + 1, tailLeft, tailRight )
        case ( LinearSeq( headLeft, _* ), _ ) =>
          object_( ValueDifference( D.value.show( headLeft ), eos ) ).some
        case ( _, LinearSeq( headRight, _* ) ) =>
          object_( ValueDifference( eos, D.value.show( headRight ) ) ).some
        case _ => none
      }
    }

    def name: String

    final override def diff( left: L, right: L ): Option[ObjectDifference] =
      diffIx( 0, left, right )
  }

  def listDiff[A, D <: Difference]( implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[List[A], ObjectDifference] =
    new LinearSeqDiffShow[A, List[A], D]( D ) {
      override def name                       = "List"
      override def show( a: List[A] ): String = a.map( D.value.show ).mkString( "List(", ", ", ")" )
    }

  def queueDiff[A, D <: Difference]( implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Queue[A], ObjectDifference] =
    new LinearSeqDiffShow[A, Queue[A], D]( D ) {
      override def name: String                = "Queue"
      override def show( a: Queue[A] ): String = a.map( D.value.show ).mkString( "Queue(", ", ", ")" )
    }

  def streamDiff[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Stream[A], ObjectDifference] =
    new LinearSeqDiffShow[A, Stream[A], D]( D ) {
      override def name: String = "Stream"
      override def show( a: Stream[A] ): String = a match {
        case h #:: _ => s"${D.value.show( h )} #:: ..."
        case _       => "Stream()"
      }
    }

  def vectorDiffShow[A, D <: Difference](
      implicit D: Lazy[DiffShow.Aux[A, D]] ): DiffShow.Aux[Vector[A], ObjectDifference] =
    new DiffShow[Vector[A]] {

      override type Out = ObjectDifference

      override def show( a: Vector[A] ): String = a.map( D.value.show ).mkString( "Vector(", ", ", ")" )

      override def diff( left: Vector[A], right: Vector[A] ): Option[ObjectDifference] = diffIx( 0, left, right )

      @tailrec
      private def diffIx( ix: Int, left: Vector[A], right: Vector[A] ): Option[ObjectDifference] = {
        def tagged( d: Difference ): TaggedDifference  = TaggedDifference( s"[$ix]", d )
        def object_( d: Difference ): ObjectDifference = ObjectDifference( "Vector", NonEmptyList.of( tagged( d ) ) )
        def eos: String                                = s"<end of Vector>"

        ( left.isDefinedAt( ix ), right.isDefinedAt( ix ) ) match {
          case ( false, false ) => none
          case ( true, false )  => object_( ValueDifference( D.value.show( left( ix ) ), eos ) ).some
          case ( false, true )  => object_( ValueDifference( eos, D.value.show( right( ix ) ) ) ).some
          case ( true, true ) =>
            val d = D.value.diff( left( ix ), right( ix ) )
            if (d.isDefined)
              d.map( object_ )
            else
              diffIx( ix + 1, left, right )
        }
      }
    }

  // TODO, maybe: Range, NumericRange

}
