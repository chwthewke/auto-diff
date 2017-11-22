package fr.thomasdufour.autodiff

import cats.syntax.either._
import mouse.all._

object util {
  private def dropUntilLast( sep: String )( s: String ): String =
    s.substring( s.lastIndexOf( sep ) + 1 )

  private[autodiff] def getClassSimpleName( klass: Class[_] ): String =
    Either
      .catchOnly[InternalError]( klass.getSimpleName )
      .recoverWith {
        case _ =>
          Either.catchNonFatal(
            klass.getName.stripSuffix( "$" )
              |> dropUntilLast( "." )
              |> dropUntilLast( "$" ) )
      }
      .getOrElse( "<unknown>" )

  def red( s: String ): String = Console.RED + s + Console.RESET

  def green( s: String ): String = Console.GREEN + s + Console.RESET
}
