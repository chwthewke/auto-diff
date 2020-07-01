package fr.thomasdufour.autodiff

import cats.Contravariant
import cats.Eq
import cats.Show

trait Hint[A] { self =>
  def apply( left: A, right: A ): Boolean
  def isDefault: Boolean = false
  def show( a: A ): String

  def contramap[B]( f: B => A ): Hint[B] =
    new Hint[B] {
      override def apply( left: B, right: B ): Boolean = self.apply( f( left ), f( right ) )
      override def show( b: B ): String                = self.show( f( b ) )

      override def isDefault: Boolean = self.isDefault
    }
}

object Hint {
  implicit def defaultMatchHint[A]: Hint[A] = new Hint[A] {
    override def apply( left: A, right: A ): Boolean = true

    override def show( a: A ): String = a.toString // weak

    override def isDefault: Boolean = true
  }

  def byEqShow[A]( implicit E: Eq[A], S: Show[A] ): Hint[A] = new Hint[A] {
    override def apply( left: A, right: A ): Boolean = E.eqv( left, right )

    override def show( a: A ): String = S.show( a )

    override def isDefault: Boolean = false
  }

  def byDiff[A]( implicit D: Diff[A] ): Hint[A] = new Hint[A] {
    override def apply( left: A, right: A ): Boolean = D.apply( left, right ).isEmpty

    override def show( a: A ): String = D.show( a )
  }

  def instance[A]( pred: ( A, A ) => Boolean, showA: A => String ): Hint[A] = new Hint[A] {
    override def apply( left: A, right: A ): Boolean = pred( left, right )

    override def show( a: A ): String = showA( a )

    override def isDefault: Boolean = false
  }

  implicit val HintContravariant: Contravariant[Hint] = new Contravariant[Hint] {
    override def contramap[A, B]( fa: Hint[A] )( f: B => A ): Hint[B] = fa.contramap( f )
  }
}
