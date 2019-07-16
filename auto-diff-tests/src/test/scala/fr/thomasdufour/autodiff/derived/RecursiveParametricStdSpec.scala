package fr.thomasdufour.autodiff
package derived

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.data.Validated
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListSet
import scala.collection.immutable.Queue
import scala.collection.immutable.TreeSet

class RecursiveParametricStdSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import RecursiveParametricStdSpec._
  import DiffOps._

  "Deriving a Diff for a recursive case class" which {

    "has recursion under a Tuple" should {
      val diff: Diff[TupleRec] = {
        import auto._
        semi.diff
      }

      "use the provided Tuple Diff" in {

        diff(
          TupleRec( Some( ( TupleRec( None ), TupleRec( Some( ( TupleRec( None ), TupleRec( None ) ) ) ) ) ) ),
          TupleRec( Some( ( TupleRec( None ), TupleRec( None ) ) ) )
        ).tree should ===(
          F(
            "TupleRec",
            "rec" -> T(
              T.Coproduct,
              "Option",
              I(
                T.Tuple,
                "Tuple2",
                2 -> F(
                  "TupleRec",
                  "rec" -> T( T.Coproduct, "Option", "Some((TupleRec(rec: None), TupleRec(rec: None)))" !== "None" )
                )
              )
            )
          )
        )
      }
    }

    "has recursion under an Option" should {
      val diff: Diff[OptionRec] = {
        import auto._
        semi.diff
      }

      "use the provided Option diff" in {

        diff(
          OptionRec( Some( OptionRec( None ) ) ),
          OptionRec( Some( OptionRec( Some( OptionRec( None ) ) ) ) )
        ).tree should ===(
          F(
            "OptionRec",
            "rec" -> T(
              T.Coproduct,
              "Option",
              F( "OptionRec", "rec" -> T( T.Coproduct, "Option", "None" !== "Some(OptionRec(rec: None))" ) )
            )
          )
        )

      }
    }

    "has recursion under an Either" should {
      val diff: Diff[EitherRec] = {
        import auto._
        semi.diff
      }

      "use the provided either diff" in {
        diff(
          EitherRec( Right( EitherRec( Left( "foo" ) ) ) ),
          EitherRec( Right( EitherRec( Left( "bar" ) ) ) ),
        ).tree should ===(
          F(
            "EitherRec",
            "rec" -> T( T.Coproduct, "Right", F( "EitherRec", "rec" -> T( T.Coproduct, "Left", "foo" !== "bar" ) ) )
          )
        )
      }
    }

    "has recursion under a Validated" should {
      val diff: Diff[ValidatedRec] = {
        import auto._
        semi.diff
      }

      "use the provided either diff" in {
        diff(
          ValidatedRec( Validated.Valid( ValidatedRec( Validated.Invalid( "foo" ) ) ) ),
          ValidatedRec( Validated.Valid( ValidatedRec( Validated.Invalid( "bar" ) ) ) ),
        ).tree should ===(
          F(
            "ValidatedRec",
            "rec" -> T(
              T.Coproduct,
              "Valid",
              F( "ValidatedRec", "rec" -> T( T.Coproduct, "Invalid", "foo" !== "bar" ) )
            )
          )
        )
      }
    }

    "has recursion under a List" should {

      val diff: Diff[ListRec] = {
        import auto._
        semi.diff
      }

      "use the provided List diff" in {

        diff(
          ListRec( ListRec( Nil ) :: ListRec( ListRec( Nil ) :: Nil ) :: Nil ),
          ListRec( ListRec( Nil ) :: ListRec( Nil ) :: Nil )
        ).tree should ===(
          F(
            "ListRec",
            "rec" -> I(
              T.Seq,
              "List",
              1 -> F( "ListRec", "rec" -> I( T.Seq, "List", 0 -> ("ListRec(rec: List())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Queue" should {

      val diff: Diff[QueueRec] = {
        import auto._
        semi.diff
      }

      val empty = Queue.empty

      "use the provided Queue diff" in {

        diff(
          QueueRec( Queue( QueueRec( empty ), QueueRec( Queue( QueueRec( empty ) ) ) ) ),
          QueueRec( Queue( QueueRec( empty ), QueueRec( empty ) ) )
        ).tree should ===(
          F(
            "QueueRec",
            "rec" -> I(
              T.Seq,
              "Queue",
              1 -> F( "QueueRec", "rec" -> I( T.Seq, "Queue", 0 -> ("QueueRec(rec: Queue())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Stream" should {

      val diff: Diff[StreamRec] = {
        import auto._
        semi.diff
      }

      val empty = Stream.empty

      "use the provided Stream diff" in {

        diff(
          StreamRec( Stream( StreamRec( empty ), StreamRec( Stream( StreamRec( empty ) ) ) ) ),
          StreamRec( Stream( StreamRec( empty ), StreamRec( empty ) ) )
        ).tree should ===(
          F(
            "StreamRec",
            "rec" -> I(
              T.Seq,
              "Stream",
              1 -> F( "StreamRec", "rec" -> I( T.Seq, "Stream", 0 -> ("StreamRec(rec: Stream())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Vector" should {

      val diff: Diff[VectorRec] = {
        import auto._
        semi.diff
      }

      val empty = Vector.empty

      "use the provided Vector diff" in {

        diff(
          VectorRec( Vector( VectorRec( empty ), VectorRec( Vector( VectorRec( empty ) ) ) ) ),
          VectorRec( Vector( VectorRec( empty ), VectorRec( empty ) ) )
        ).tree should ===(
          F(
            "VectorRec",
            "rec" -> I(
              T.Seq,
              "Vector",
              1 -> F( "VectorRec", "rec" -> I( T.Seq, "Vector", 0 -> ("VectorRec(rec: Vector())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Array" should {

      val diff: Diff[ArrayRec] = {
        import auto._
        semi.diff
      }

      val empty = Array.empty[ArrayRec]

      "use the provided Array diff" in {

        diff(
          ArrayRec( Array( ArrayRec( empty ), ArrayRec( Array( ArrayRec( empty ) ) ) ) ),
          ArrayRec( Array( ArrayRec( empty ), ArrayRec( empty ) ) )
        ).tree should ===(
          F(
            "ArrayRec",
            "rec" -> I(
              T.Seq,
              "Array",
              1 -> F( "ArrayRec", "rec" -> I( T.Seq, "Array", 0 -> ("ArrayRec(rec: Array())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Set" should {

      val diff: Diff[SetRec] = {
        import auto._
        semi.diff
      }

      val empty = Set.empty[SetRec]

      "use the provided Set diff" in {

        diff(
          SetRec( Set( SetRec( empty ), SetRec( Set( SetRec( empty ) ) ) ) ),
          SetRec( Set( SetRec( empty ) ) )
        ).tree should ===(
          F(
            "SetRec",
            "rec" -> T( T.Set, "Set", U( Some( "SetRec(rec: { SetRec(rec: {  }) })" !== "" ), Nil ) )
          )
        )

      }
    }

    "has recursion under a ListSet" should {

      val diff: Diff[ListSetRec] = {
        import auto._
        semi.diff
      }

      val empty = ListSet.empty[ListSetRec]

      "use the provided ListSet diff" in {

        diff(
          ListSetRec( ListSet( ListSetRec( empty ), ListSetRec( ListSet( ListSetRec( empty ) ) ) ) ),
          ListSetRec( ListSet( ListSetRec( empty ) ) )
        ).tree should ===(
          F(
            "ListSetRec",
            "rec" -> T( T.Set, "ListSet", U( Some( "ListSetRec(rec: { ListSetRec(rec: {  }) })" !== "" ), Nil ) )
          )
        )

      }
    }

    "has recursion under a HashSet" should {

      val diff: Diff[HashSetRec] = {
        import auto._
        semi.diff
      }

      val empty = HashSet.empty[HashSetRec]

      "use the provided HashSet diff" in {

        diff(
          HashSetRec( HashSet( HashSetRec( empty ), HashSetRec( HashSet( HashSetRec( empty ) ) ) ) ),
          HashSetRec( HashSet( HashSetRec( empty ) ) )
        ).tree should ===(
          F(
            "HashSetRec",
            "rec" -> T( T.Set, "HashSet", U( Some( "HashSetRec(rec: { HashSetRec(rec: {  }) })" !== "" ), Nil ) )
          )
        )

      }
    }

    "has recursion under a TreeSet" should {

      val diff: Diff[TreeSetRec] = {
        import auto._
        semi.diff
      }

      val empty = TreeSet.empty[TreeSetRec]

      "use the provided TreeSet diff" in {

        diff(
          TreeSetRec( TreeSet( TreeSetRec( empty ), TreeSetRec( TreeSet( TreeSetRec( empty ) ) ) ) ),
          TreeSetRec( TreeSet( TreeSetRec( empty ) ) )
        ).tree should ===(
          F(
            "TreeSetRec",
            "rec" -> T( T.Set, "TreeSet", U( Some( "TreeSetRec(rec: { TreeSetRec(rec: {  }) })" !== "" ), Nil ) )
          )
        )

      }
    }

    "has recursion under a Chain" should {

      val diff: Diff[ChainRec] = {
        import auto._
        semi.diff
      }

      val empty = Chain.empty

      "use the provided Chain diff" in {

        diff(
          ChainRec( Chain( ChainRec( empty ), ChainRec( Chain( ChainRec( empty ) ) ) ) ),
          ChainRec( Chain( ChainRec( empty ), ChainRec( empty ) ) )
        ).tree should ===(
          F(
            "ChainRec",
            "rec" -> I(
              T.Seq,
              "Chain",
              1 -> F( "ChainRec", "rec" -> I( T.Seq, "Chain", 0 -> ("ChainRec(rec: Chain())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a NonEmptyChain" should {

      val diff: Diff[NecRec] = {
        import auto._
        semi.diff
      }

      "use the provided NonEmptyChain diff" in {

        diff(
          NecRec( Some( NonEmptyChain( NecRec( None ), NecRec( Some( NonEmptyChain( NecRec( None ) ) ) ) ) ) ),
          NecRec( Some( NonEmptyChain( NecRec( None ), NecRec( None ) ) ) )
        ).tree should ===(
          F(
            "NecRec",
            "rec" ->
              T(
                T.Coproduct,
                "Option",
                I(
                  T.Seq,
                  "NonEmptyChain",
                  1 -> F(
                    "NecRec",
                    "rec" ->
                      T( T.Coproduct, "Option", "Some(NonEmptyChain(NecRec(rec: None)))" !== "None" )
                  )
                )
              )
          )
        )

      }
    }

    "has recursion under a NonEmptyList" should {

      val diff: Diff[NelRec] = {
        import auto._
        semi.diff
      }

      "use the provided NonEmptyList diff" in {

        diff(
          NelRec( Some( NonEmptyList.of( NelRec( None ), NelRec( Some( NonEmptyList.of( NelRec( None ) ) ) ) ) ) ),
          NelRec( Some( NonEmptyList.of( NelRec( None ), NelRec( None ) ) ) )
        ).tree should ===(
          F(
            "NelRec",
            "rec" ->
              T(
                T.Coproduct,
                "Option",
                I(
                  T.Seq,
                  "NonEmptyList",
                  1 -> F(
                    "NelRec",
                    "rec" ->
                      T( T.Coproduct, "Option", "Some(NonEmptyList(NelRec(rec: None)))" !== "None" )
                  )
                )
              )
          )
        )

      }
    }

    "has recursion under a NonEmptyVector" should {

      val diff: Diff[NevRec] = {
        import auto._
        semi.diff
      }

      "use the provided NonEmptyVector diff" in {

        diff(
          NevRec( Some( NonEmptyVector.of( NevRec( None ), NevRec( Some( NonEmptyVector.of( NevRec( None ) ) ) ) ) ) ),
          NevRec( Some( NonEmptyVector.of( NevRec( None ), NevRec( None ) ) ) )
        ).tree should ===(
          F(
            "NevRec",
            "rec" ->
              T(
                T.Coproduct,
                "Option",
                I(
                  T.Seq,
                  "NonEmptyVector",
                  1 -> F(
                    "NevRec",
                    "rec" ->
                      T( T.Coproduct, "Option", "Some(NonEmptyVector(NevRec(rec: None)))" !== "None" )
                  )
                )
              )
          )
        )

      }
    }

  }

  // TODO: a bunch more

}

object RecursiveParametricStdSpec {
  case class TupleRec( rec: Option[( TupleRec, TupleRec )] )

  case class OptionRec( rec: Option[OptionRec] )
  case class EitherRec( rec: Either[String, EitherRec] )
  case class ValidatedRec( rec: Validated[String, ValidatedRec] )
  case class ListRec( rec: List[ListRec] )
  case class QueueRec( rec: Queue[QueueRec] )
  case class StreamRec( rec: Stream[StreamRec] )
  case class VectorRec( rec: Vector[VectorRec] )
  case class ArrayRec( rec: Array[ArrayRec] )
  case class SetRec( rec: Set[SetRec] )
  case class ListSetRec( rec: ListSet[ListSetRec] )
  case class HashSetRec( rec: HashSet[HashSetRec] )
  case class TreeSetRec( rec: TreeSet[TreeSetRec] )
  object TreeSetRec {
    implicit val treeSetRecOrder: Order[TreeSetRec] =
      new Order[TreeSetRec] {
        override def compare( x: TreeSetRec, y: TreeSetRec ): Int = {
          val cmpSize = x.rec.size.compareTo( y.rec.size )
          if (cmpSize != 0)
            cmpSize
          else
            x.rec.toIterator
              .zip( y.rec.toIterator )
              .map( (compare _).tupled )
              .find( _ != 0 )
              .getOrElse( 0 )
        }
      }
  }

  case class ChainRec( rec: Chain[ChainRec] )
  case class NecRec( rec: Option[NonEmptyChain[NecRec]] )
  case class NelRec( rec: Option[NonEmptyList[NelRec]] )
  case class NevRec( rec: Option[NonEmptyVector[NevRec]] )

}
