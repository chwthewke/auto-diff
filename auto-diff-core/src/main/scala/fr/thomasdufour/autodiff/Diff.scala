package fr.thomasdufour.autodiff

import cats.Contravariant
import cats.Eval
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.data.Validated
import cats.kernel.Eq
import cats.syntax.option._
import com.github.ghik.silencer.silent
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.ListSet
import scala.collection.immutable.LongMap
import scala.collection.immutable.Queue
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet

trait Diff[A] { self =>
  def apply( left: A, right: A ): Option[Difference]

  def show( value: A ): String

  def contramap[B]( f: B => A ): Diff[B] =
    Diff.instance( ( l, r ) => self.apply( f( l ), f( r ) ), v => self.show( f( v ) ) )
}

object Diff extends TupleDiff with ProductDiff with DiffVersionSpecific {
  def apply[A]( implicit ev: Diff[A] ): Diff[A] = ev

  def instance[A]( diff: ( A, A ) => Option[Difference], show0: A => String ): Diff[A] =
    new Diff[A] {
      override def apply( left: A, right: A ): Option[Difference] = diff( left, right )

      override def show( value: A ): String = show0( value )
    }

  def ignore[A]: Diff[A] = instance( ( _, _ ) => None, _ => "(ignored)" )

  implicit val diffContravariant: Contravariant[Diff] = new Contravariant[Diff] {
    override def contramap[A, B]( fa: Diff[A] )( f: B => A ): Diff[B] = fa.contramap( f )
  }

  def explicitEqShow[A]( eqv: ( A, A ) => Boolean, show: A => String ): Diff[A] = {
    def diff( l: A, r: A ): Option[Difference] =
      if (eqv( l, r )) none
      else Difference.Value( show( l ), show( r ) ).some

    Diff.instance( diff, show )
  }

  def implicitEqShow[A]( implicit E: Eq[A], S: Show[A] ): Diff[A] =
    explicitEqShow( E.eqv, S.show )

  def defaultEqShow[A]: Diff[A] =
    explicitEqShow( _ == _, _.toString )

  // basic types
  implicit val booleanDiff: Diff[Boolean] = defaultEqShow
  implicit val byteDiff: Diff[Byte]       = defaultEqShow
  implicit val shortDiff: Diff[Short]     = defaultEqShow
  implicit val intDiff: Diff[Int]         = defaultEqShow
  implicit val longDiff: Diff[Long]       = defaultEqShow
  implicit val floatDiff: Diff[Float]     = defaultEqShow
  implicit val doubleDiff: Diff[Double]   = defaultEqShow
  implicit val charDiff: Diff[Char]       = defaultEqShow
  implicit val stringDiff: Diff[String]   = defaultEqShow
  implicit val uuidDiff: Diff[UUID]       = defaultEqShow
  implicit val unitDiff: Diff[Unit]       = defaultEqShow

  // java.time types
  implicit val instantDiff: Diff[Instant]               = defaultEqShow
  implicit val localDateDiff: Diff[LocalDate]           = defaultEqShow
  implicit val localTimeDiff: Diff[LocalTime]           = defaultEqShow
  implicit val localDateTime: Diff[LocalDateTime]       = defaultEqShow
  implicit val offsetTimeDiff: Diff[OffsetTime]         = defaultEqShow
  implicit val offsetDateTimeDiff: Diff[OffsetDateTime] = defaultEqShow
  implicit val zonedDateTimeDiff: Diff[ZonedDateTime]   = defaultEqShow

  implicit def evalDiff[A]( implicit D: Diff[A] ): Diff[Eval[A]] = D.contramap( _.value )

  implicit def optionDiff[A]( implicit D: Diff[A] ): Diff[Option[A]] = new Diff[Option[A]] {
    override def show( value: Option[A] ): String =
      value.fold( "None" )( a => s"Some(${D.show( a )})" )

    override def apply( left: Option[A], right: Option[A] ): Option[Difference] = ( left, right ) match {
      case ( None, None )           => None
      case ( Some( l ), Some( r ) ) => D.apply( l, r ).map( Difference.Coproduct( "Option", _ ) )
      case ( _, _ )                 => Difference.Coproduct( "Option", Difference.Value( show( left ), show( right ) ) ).some
    }
  }

  implicit def eitherDiff[A, B]( implicit DA: Diff[A], DB: Diff[B] ): Diff[Either[A, B]] = new Diff[Either[A, B]] {
    override def show( value: Either[A, B] ): String =
      value.fold( a => s"Left(${DA.show( a )})", b => s"Right(${DB.show( b )})" )

    override def apply( left: Either[A, B], right: Either[A, B] ): Option[Difference] = ( left, right ) match {
      case ( Left( l ), Left( r ) )   => DA.apply( l, r ).map( Difference.Coproduct( "Left", _ ) )
      case ( Right( l ), Right( r ) ) => DB.apply( l, r ).map( Difference.Coproduct( "Right", _ ) )
      case ( _, _ )                   => Difference.Coproduct( "Either", Difference.Value( show( left ), show( right ) ) ).some
    }
  }

  implicit def validatedDiff[A, B]( implicit DA: Diff[A], DB: Diff[B] ): Diff[Validated[A, B]] =
    new Diff[Validated[A, B]] {
      import Validated.Invalid
      import Validated.Valid

      override def show( value: Validated[A, B] ): String =
        value.fold( a => s"Invalid(${DA.show( a )})", b => s"Valid(${DB.show( b )})" )

      override def apply( left: Validated[A, B], right: Validated[A, B] ): Option[Difference] = ( left, right ) match {
        case ( Invalid( l ), Invalid( r ) ) => DA.apply( l, r ).map( Difference.Coproduct( "Invalid", _ ) )
        case ( Valid( l ), Valid( r ) )     => DB.apply( l, r ).map( Difference.Coproduct( "Valid", _ ) )
        case ( _, _ )                       => Difference.Coproduct( "Validated", Difference.Value( show( left ), show( right ) ) ).some
      }
    }

  def inAnyOrder[A, CC[_]]( implicit D: Diff[A], H: Hint[A], D1: AsIterable[CC] ): Diff[CC[A]] =
    InAnyOrder.anyOrderDiff[A].contramap( cc => InAnyOrder.diffable( cc ) )

  implicit def listDiff[A]( implicit D: Diff[A] ): Diff[List[A]]   = LinearSeqDiff.listDiff
  implicit def queueDiff[A]( implicit D: Diff[A] ): Diff[Queue[A]] = LinearSeqDiff.queueDiff
  @silent( "deprecated" )
  implicit def streamDiff[A]( implicit D: Diff[A] ): Diff[Stream[A]] = LinearSeqDiff.streamDiff
  implicit def vectorDiff[A]( implicit D: Diff[A] ): Diff[Vector[A]] = IndexedSeqDiff.vectorDiff

  implicit def setDiff[A]( implicit D: Diff[A] ): Diff[Set[A]]             = SetDiff.setDiff
  implicit def sortedSetDiff[A]( implicit D: Diff[A] ): Diff[SortedSet[A]] = SetDiff.sortedSetDiff
  implicit def listSetDiff[A]( implicit D: Diff[A] ): Diff[ListSet[A]]     = SetDiff.listSetDiff
  implicit def hashSetDiff[A]( implicit D: Diff[A] ): Diff[HashSet[A]]     = SetDiff.hashSetDiff
  implicit def treeSetDiff[A]( implicit D: Diff[A] ): Diff[TreeSet[A]]     = SetDiff.treeSetDiff

  // TODO More collections (Ranges, BitSets)

  implicit def mapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[Map[K, V]]             = MapDiff.mapDiff
  implicit def sortedMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[SortedMap[K, V]] = MapDiff.sortedMapDiff
  implicit def intMapDiff[V]( implicit DV: Diff[V] ): Diff[IntMap[V]]                          = MapDiff.intMapDiff
  implicit def longMapDiff[V]( implicit DV: Diff[V] ): Diff[LongMap[V]]                        = MapDiff.longMapDiff
  implicit def hashMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[HashMap[K, V]]     = MapDiff.hashMapDiff
  implicit def listMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[ListMap[K, V]]     = MapDiff.listMapDiff
  implicit def treeMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[TreeMap[K, V]]     = MapDiff.treeMapDiff

  // cats-data collections

  implicit def chainDiff[A]( implicit D: Diff[A] ): Diff[Chain[A]]        = CatsDataDiff.chainDiff
  implicit def necDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyChain[A]]  = CatsDataDiff.nonEmptyChainDiff
  implicit def nelDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyList[A]]   = CatsDataDiff.nonEmptyListDiff
  implicit def nevDiff[A]( implicit D: Diff[A] ): Diff[NonEmptyVector[A]] = CatsDataDiff.nonEmptyVectorDiff
  implicit def nesDiff[A]( implicit D: Diff[A] ): Diff[NonEmptySet[A]]    = CatsDataDiff.nonEmptySetDiff

  implicit def nemDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[NonEmptyMap[K, V]] =
    CatsDataDiff.nonEmptyMapDiff

  implicit def iterableDiff[A]( implicit D: Diff[A] ): Diff[Iterable[A]] =
    LinearSeqDiff.diffAsList[A]( "an iterable" ).contramap[Iterable[A]]( _.toList )

}
