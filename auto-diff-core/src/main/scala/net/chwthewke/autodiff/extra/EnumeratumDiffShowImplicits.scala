package net.chwthewke.autodiff.extra

import _root_.enumeratum.Enum
import _root_.enumeratum.EnumEntry
import net.chwthewke.autodiff.DiffShow
import net.chwthewke.autodiff.ExportedDiffShow

trait EnumeratumDiffShowImplicits {
  implicit def enumDiffShow[A <: EnumEntry]( implicit E: Enum[A] ): ExportedDiffShow[A] = {
    val _ = E
    new ExportedDiffShow( DiffShow.fromEquality[A]( _ == _, _.entryName ) )
  }
}
