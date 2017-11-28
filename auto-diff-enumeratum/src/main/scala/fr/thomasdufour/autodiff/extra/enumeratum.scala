package fr.thomasdufour.autodiff
package extra

import _root_.enumeratum.Enum
import _root_.enumeratum.EnumEntry

trait EnumeratumDiffShowImplicits {
  implicit def enumDiffShow[A <: EnumEntry]( implicit E: Enum[A] ): ExportedDiffShow[A] = {
    val _ = E
    new ExportedDiffShow( DiffShow.fromEquality[A]( _ == _, _.entryName ) )
  }
}

object enumeratum extends EnumeratumDiffShowImplicits
