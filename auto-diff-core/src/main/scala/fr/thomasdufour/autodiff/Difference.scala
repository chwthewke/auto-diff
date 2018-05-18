package fr.thomasdufour.autodiff

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.reducible._

sealed trait Difference

object Difference {

  final case class Value[A]( left: A, right: A, show: A => String )                 extends Difference
  final case class Tagged( tag: String, diff: Difference )                          extends Difference
  final case class Expected[A]( left: A, right: String, show: A => String )         extends Difference
  final case class Unexpected[A]( left: String, right: A, show: A => String )       extends Difference
  final case class Coproduct( name: String, difference: Difference )                extends Difference
  final case class Product( name: String, fields: NonEmptyList[Field] )             extends Difference
  final case class Tuple( name: String, fields: NonEmptyList[Index] )               extends Difference
  final case class Seq( name: String, diffs: NonEmptyList[Index] )                  extends Difference
  final case class Set( name: String, diff: Difference )                            extends Difference
  final case class Map[K]( name: String, keys: Option[Set], diffs: List[Keyed[K]] ) extends Difference

  final case class Field( name: String, difference: Difference )
  final case class Keyed[K]( key: K, showKey: K => String, difference: Difference )
  final case class Index( index: Int, difference: Difference )

  case class Line( val line: String ) extends AnyVal
  object Line {
    implicit val lineSemigroup: Semigroup[Line] = new Semigroup[Line] {
      @inline
      override def combine( x: Line, y: Line ): Line = Line( x.line + "\n" + y.line )
    }

  }

  implicit class LineOps( val self: ( String, Int ) ) extends AnyVal {
    def indent( width: Int ): Line =
      Line( (if (self._2 == 0) "" else (" " * (width * (self._2 - 1)) + "- ")) + self._1 )
  }

  case class Pretty( indentWidth: Int, color: Boolean ) {
    private def colorize( ansi: String )( str: String ): String =
      if (color) ansi + str + Console.RESET else str

    private def indent( line: ( String, Int ) ): ( String, Int ) =
      line match { case ( str, ind ) => ( str, ind + 1 ) }

    private def prettyValueDiff[A]( left: String, right: String ): String =
      colorize( Console.GREEN )( left ) + " -> " + colorize( Console.RED )( right )

    private def prettyIndented( ind: Int, diff: Difference ): NonEmptyList[( String, Int )] = diff match {
      case Value( l, r, s )      => NonEmptyList.of( ( prettyValueDiff( s( l ), s( r ) ), ind ) )
      case Tagged( t, d )        => ( t, ind ) :: prettyIndented( ind + 1, d )
      case Expected( l, r, s )   => NonEmptyList.of( ( prettyValueDiff( s( l ), r ), ind ) )
      case Unexpected( l, r, s ) => NonEmptyList.of( ( prettyValueDiff( l, s( r ) ), ind ) )
      case Coproduct( n, d )     => ( s"in $n", ind ) :: prettyIndented( ind + 1, d )
      case Product( n, fs )      => ( s"in $n", ind ) :: fs.reduceMap( prettyField( ind + 1, _ ) )
      case Tuple( n, ixs )       => ( s"in $n", ind ) :: ixs.reduceMap( prettyIndex( ind + 1, _ ) )
      case Seq( n, ixs )         => ( s"in $n", ind ) :: ixs.reduceMap( prettyIndex( ind + 1, _ ) )
      case Set( n, d )           => ( s"in $n", ind ) :: prettyIndented( ind + 1, d )
      case Map( n, ks, ds ) =>
        NonEmptyList( ( s"in $n", ind ),
                     (ks.foldMap( prettyIndented( ind + 1, _ ).toList ) ) ++
                       (ds.foldMap( prettyKeyed( ind + 1, _ ).toList ) ) )
    }

    private def prettyField( ind: Int, f: Field ): NonEmptyList[( String, Int )] =
      ( f.name, ind ) :: prettyIndented( ind + 1, f.difference )

    private def prettyIndex( ind: Int, ix: Index ): NonEmptyList[( String, Int )] =
      ( s"[${ix.index}]", ind ) :: prettyIndented( ind + 1, ix.difference )

    private def prettyKeyed[K]( ind: Int, keyed: Keyed[K] ): NonEmptyList[( String, Int )] =
      ( s"at ${keyed.showKey( keyed.key )}", ind ) :: prettyIndented( ind + 1, keyed.difference )

    def show( d: Difference ): String = prettyIndented( 0, d ).reduceMap( _.indent( indentWidth ) ).line

  }

  object Pretty {
    val Plain2: Pretty     = Pretty( indentWidth = 2, color = false )
    val Colorized2: Pretty = Pretty( indentWidth = 2, color = true )
  }

}
