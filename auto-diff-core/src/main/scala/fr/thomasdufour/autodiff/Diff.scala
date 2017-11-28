package fr.thomasdufour.autodiff

object Diff {
  def apply[A]( implicit ev: DiffShow[A] { type Out <: Difference } ): Diff[A] = ev

  def by[A, B]( f: A => B )( implicit D: Diff[B] ): DiffShow.Aux[A, D.Out] = D.contramap( f )

  def inAnyOrder[A, CC[_]]( implicit D: Diff[unordered.AnyOrder[A]], ev: CC[A] <:< Traversable[A] ): Diff[CC[A]] =
    Diff[unordered.AnyOrder[A]].contramap[CC[A]]( cc => unordered.inAnyOrder( ev( cc ) ) )

  class Ops[A]( val self: Diff[A] ) extends AnyVal {
    def apply( left: A, right: A ): Option[Difference] = self.diff( left, right )
  }
}
