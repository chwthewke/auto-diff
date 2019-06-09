package fr.thomasdufour.autodiff
package derived

object semi {
  def diff[A]( implicit ev: MkDiff[A] ): Diff[A] = ev
}
