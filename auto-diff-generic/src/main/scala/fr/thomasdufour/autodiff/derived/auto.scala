package fr.thomasdufour.autodiff
package derived

import shapeless.Lazy
import shapeless.Refute

object auto {
  implicit def mkDiff[A]( implicit refute: Refute[Diff[A]], ev: Lazy[MkDiff[A]] ): Diff[A] = {
    val _ = refute
    ev.value
  }
}
