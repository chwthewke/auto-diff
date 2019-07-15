package fr.thomasdufour.autodiff

import cats.data.NonEmptyList

/**
  * Untyped difference trees used as a testable representation of Difference.
  */
// TODO It made sense once, but now, it might be better/simpler to replace with extractors, wouldn't it?
sealed trait DifferenceTree

case object Z                                                             extends DifferenceTree
final case class V( left: String, right: String )                         extends DifferenceTree
final case class U( d: Option[DifferenceTree], ms: List[DifferenceTree] ) extends DifferenceTree
final case class T( kind: T.K, name: String, d: DifferenceTree )          extends DifferenceTree
final case class I( kind: T.I, name: String, ds: NonEmptyList[T.Index] )  extends DifferenceTree
final case class F( name: String, ds: NonEmptyList[T.Field] )             extends DifferenceTree
final case class M( name: String, keys: Option[T], ds: List[T.Keyed] )    extends DifferenceTree

object I {
  def apply( kind: T.I, name: String, index0: ( Int, DifferenceTree ), indexes: ( Int, DifferenceTree )* ): I = {
    def toIndex( ix: ( Int, DifferenceTree ) ): T.Index = (T.Index( _, _ ) ).tupled( ix )
    I( kind, name, NonEmptyList.of( toIndex( index0 ), indexes.map( toIndex ): _* ) )
  }
}

object F {
  def apply( name: String, field0: ( String, DifferenceTree ), fields: ( String, DifferenceTree )* ): F = {
    def toField( fd: ( String, DifferenceTree ) ): T.Field = (T.Field( _, _ ) ).tupled( fd )
    F( name, NonEmptyList.of( toField( field0 ), fields.map( toField ): _* ) )
  }
}

object M {
  def toKeyed( kd: ( String, DifferenceTree ) ): T.Keyed = (T.Keyed( _, _ ) ).tupled( kd )

  def apply( name: String, keys: T, ds: ( String, DifferenceTree )* ): M =
    M( name, Some( keys ), ds.toList.map( toKeyed ) )

  def apply( name: String, ds: ( String, DifferenceTree )* ): M =
    M( name, None, ds.toList.map( toKeyed ) )
}

object T {
  sealed trait K
  case object Coproduct extends K
  case object Set       extends K
  case object Tag       extends K

  sealed trait I
  case object Tuple extends I
  case object Seq   extends I

  case class Field( name: String, d: DifferenceTree )
  case class Index( ix: Int, d: DifferenceTree )
  case class Keyed( key: String, d: DifferenceTree )
}

object DifferenceTree {
  import Difference._

  def fromDifference( d: Difference ): DifferenceTree = d match {
    case Value( l, r )     => V( l, r )
    case Tagged( t, d )    => T( T.Tag, t, fromDifference( d ) )
    case Coproduct( n, d ) => T( T.Coproduct, n, fromDifference( d ) )
    case Product( n, fs )  => F( n, fs.map( f => T.Field( f.name, fromDifference( f.difference ) ) ) )
    case Tuple( n, fs )    => I( T.Tuple, n, fs.map( f => T.Index( f.index, fromDifference( f.difference ) ) ) )
    case Seq( n, ds )      => I( T.Seq, n, ds.map( d => T.Index( d.index, fromDifference( d.difference ) ) ) )
    case Set( n, d )       => T( T.Set, n, fromDifference( d ) )
    case Unordered( d ) =>
      U( d.left.map( fromDifference ), d.right.fold( List.empty[DifferenceTree] )( _.toList.map( fromDifference ) ) )
    case Map( n, ds ) =>
      M(
        n,
        ds.left.map( k => T( T.Set, k.name, fromDifference( k.diff ) ) ),
        ds.right
          .fold( List.empty[Keyed] )( _.toList )
          .map( d => T.Keyed( d.key, fromDifference( d.difference ) ) )
      )

  }

  def fromDifferenceOption( opt: Option[Difference] ): DifferenceTree =
    opt.fold[DifferenceTree]( Z )( fromDifference )

}
