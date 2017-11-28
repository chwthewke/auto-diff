package fr.thomasdufour.autodiff

object Diff {
  def apply[A]( implicit ev: Diff[A] ): Diff[A] = ev

  def by[A, B]( f: A => B )( implicit D: Diff[B] ): DiffShow.Aux[A, D.Out] = D.contramap( f )

  class Ops[A]( val self: Diff[A] ) extends AnyVal {
    def apply( left: A, right: A ): Option[Difference] = self.diff( left, right )
  }
}
