package fr.thomasdufour.autodiff
package extra

import _root_.enumeratum.Enum
import _root_.enumeratum.EnumEntry

trait EnumeratumDiffImplicits {
  implicit def enumDiffShow[A <: EnumEntry]( implicit E: Enum[A] ): Diff[A] = {
    val _ = E
    Diff.explicitEqShow[A]( _ == _, _.entryName )
  }
}

object enumeratum extends EnumeratumDiffImplicits
