package fr.thomasdufour

import cats.syntax.either._
import mouse.all.optionSyntaxMouse

package object autodiff {
  import scala.collection.{immutable => sci}

  private[autodiff] type Seq[A] = sci.Seq[A]
  private[autodiff] val Seq: sci.Seq.type = sci.Seq

  private[autodiff] type IndexedSeq[A] = sci.IndexedSeq[A]
  private[autodiff] val indexedSeq: sci.IndexedSeq.type = sci.IndexedSeq

  private[autodiff] type LinearSeq[A] = sci.LinearSeq[A]
  private[autodiff] val LinearSeq: sci.LinearSeq.type = sci.LinearSeq

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

}
