package fr.thomasdufour.autodiff
package generic

import scala.language.experimental.macros

import fr.thomasdufour.autodiff.macros.ExportMacros

object auto {
  implicit def exportDerivedDiff[A]: Exported[Diff[A]] = macro ExportMacros.exportDiff[A]
}
