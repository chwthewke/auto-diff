package fr.thomasdufour.autodiff
package derived

import shapeless.Cached
import shapeless.Refute

object cached {
  implicit def mkDiff[A]( implicit refute: Refute[Diff[A]], ev: Cached[MkDiff[A]] ): Diff[A] = {
    val _ = refute
    ev.value
  }
}
