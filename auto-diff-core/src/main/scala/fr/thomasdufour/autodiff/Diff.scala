package fr.thomasdufour.autodiff

import cats.Show
import cats.Contravariant
import cats.kernel.Eq
import cats.syntax.option._
import java.util.UUID
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.ListSet
import scala.collection.immutable.LongMap
import scala.collection.immutable.Map
import scala.collection.immutable.Queue
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet

trait Diff[A] { self =>
  def apply( left: A, right: A ): Option[Difference]

  def show( value: A ): String

  def contramap[B]( f: B => A ): Diff[B] =
    Diff.instance( ( l, r ) => self.apply( f( l ), f( r ) ), v => self.show( f( v ) ) )

  def mapDifference( f: Difference => Difference ): Diff[A] =
    Diff.instance( ( l, r ) => self.apply( l, r ).map( f ), self.show )

  def mapString( f: String => String ): Diff[A] =
    Diff.instance( self.apply, v => f( self.show( v ) ) )
}

object Diff extends TupleDiff with ProductDiff with MidPriorityDiffImplicits {
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
      case ( Left( l ), Left( r ) )   => DA.apply( l, r ).map( Difference.Coproduct( "Either", _ ) )
      case ( Right( l ), Right( r ) ) => DB.apply( l, r ).map( Difference.Coproduct( "Either", _ ) )
      case ( _, _ )                   => Difference.Coproduct( "Either", Difference.Value( show( left ), show( right ) ) ).some
    }
  }

  def inAnyOrder[A, CC[x] <: Iterable[x]]( implicit D: Diff[A], H: DiffMatch.Hint[A] ): Diff[CC[A]] =
    InAnyOrder.anyOrderDiff[A].contramap( cc => InAnyOrder( cc ) )

  implicit def listDiff[A]( implicit D: Diff[A] ): Diff[List[A]]     = LinearSeqDiff.listDiff
  implicit def queueDiff[A]( implicit D: Diff[A] ): Diff[Queue[A]]   = LinearSeqDiff.queueDiff
  implicit def streamDiff[A]( implicit D: Diff[A] ): Diff[Stream[A]] = LinearSeqDiff.streamDiff
  implicit def vectorDiff[A]( implicit D: Diff[A] ): Diff[Vector[A]] = IndexedSeqDiff.vectorDiff
  implicit def arrayDiff[A]( implicit D: Diff[A] ): Diff[Array[A]]   = IndexedSeqDiff.arrayDiff

  implicit def setDiff[A]( implicit D: Diff[A] ): Diff[Set[A]]         = SetDiff.setDiff
  implicit def listSetDiff[A]( implicit D: Diff[A] ): Diff[ListSet[A]] = SetDiff.listSetDiff
  implicit def hashSetDiff[A]( implicit D: Diff[A] ): Diff[HashSet[A]] = SetDiff.hashSetDiff
  implicit def treeSetDiff[A]( implicit D: Diff[A] ): Diff[TreeSet[A]] = SetDiff.treeSetDiff

  // TODO More collections (Ranges, BitSets)

  implicit def mapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[Map[K, V]]         = MapDiff.mapDiff
  implicit def intMapDiff[V]( implicit DV: Diff[V] ): Diff[IntMap[V]]                      = MapDiff.intMapDiff
  implicit def longMapDiff[V]( implicit DV: Diff[V] ): Diff[LongMap[V]]                    = MapDiff.longMapDiff
  implicit def hashMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[HashMap[K, V]] = MapDiff.hashMapDiff
  implicit def listMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[ListMap[K, V]] = MapDiff.listMapDiff
  implicit def treeMapDiff[K, V]( implicit DK: Diff[K], DV: Diff[V] ): Diff[TreeMap[K, V]] = MapDiff.treeMapDiff

}

trait MidPriorityDiffImplicits extends LowPriorityDiffImplicits {
  implicit def iterableDiff[A]( implicit D: Diff[A] ): Diff[Iterable[A]] =
    Diff[List[A]]
      .contramap[Iterable[A]]( _.toList )
      .mapDifference {
        case Difference.Seq( _, diffs ) => Difference.Seq( "an iterable", diffs )
        case d                          => d
      }
}

trait LowPriorityDiffImplicits {
  implicit def importDiff[A]( implicit exported: Exported[Diff[A]] ): Diff[A] = exported.instance
}
