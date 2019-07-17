package fr.thomasdufour.autodiff
package derived

import cats.syntax.either._

object GuessClassName {

  private[autodiff] def getClassSimpleName( klass: Class[_] ): String =
    Either
      .catchOnly[InternalError]( klass.getSimpleName )
      .getOrElse( guessSimpleClassName( klass.getName ) )

  private def guessSimpleClassName( className: String ): String = {
    val name  = dropUntilLast( "." )( className )
    val parts = name.split( '$' ).toList.reverse

    parts
      .find( p => p.nonEmpty && !p( 0 ).isDigit )
      .orElse( parts.find( p => p.nonEmpty ) )
      .getOrElse( "<unknown>" )
  }

  private def dropUntilLast( sep: String )( s: String ): String =
    s.substring( s.lastIndexOf( sep ) + 1 )
}
