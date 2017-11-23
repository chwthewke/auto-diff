package fr.thomasdufour.autodiff

import cats.Show

trait Diff[A] {
  def apply( left: A, right: A ): Option[Difference]

  def show( value: A ): String

  def diff( left: A, right: A ): Option[String] = apply( left, right ).map( Show[Difference].show )
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
