package fr.thomasdufour.autodiff
package text

import cats.Semigroup

case class Line( val line: String ) extends AnyVal

object Line {
  implicit val lineSemigroup: Semigroup[Line] = new Semigroup[Line] {
    @inline
    override def combine( x: Line, y: Line ): Line = Line( x.line + "\n" + y.line )
  }
}
