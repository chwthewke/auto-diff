package fr.thomasdufour.autodiff
package generic

import shapeless.Lazy

object semiauto {
  def deriveDiff[A]( implicit ev: Lazy[DerivedDiff[A]] ): Diff[A] = ev.value
}
