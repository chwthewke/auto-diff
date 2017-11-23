package fr.thomasdufour.autodiff

trait Diff[A] {
  def apply( left: A, right: A ): Option[Difference]

  def show( value: A ): String
}

object Diff extends LowPriorityDiffImplicits {
  def apply[A]( implicit ev: Diff[A] ): Diff[A] = ev
}

trait LowPriorityDiffImplicits {
  implicit def exportedDiffShow[A, D <: Difference]( implicit D: DiffShow.Aux[A, D] ): Diff[A] =
    new Diff[A] {
      override def apply( left: A, right: A ): Option[Difference] = D.diff( left, right )

      override def show( value: A ): String = D.show( value )
    }
}
