package fr.thomasdufour.autodiff.macros

import scala.reflect.macros.blackbox

import fr.thomasdufour.autodiff.Diff
import fr.thomasdufour.autodiff.Exported
import fr.thomasdufour.autodiff.generic.DerivedDiff

object ExportMacros {

  def exportDiff[A]( c: blackbox.Context )( implicit A: c.WeakTypeTag[A] ): c.Expr[Exported[Diff[A]]] = {
    import c.universe._

    val D: c.WeakTypeTag[DerivedDiff[_]] = implicitly

    val target = appliedType( D.tpe.typeConstructor, A.tpe )

    c.typecheck( q"_root_.shapeless.lazily[$target]", silent = true ) match {
      case EmptyTree => c.abort( c.enclosingPosition, s"Unable to infer value of type $target" )
      case t =>
        c.Expr[Exported[Diff[A]]](
          q"new _root_.fr.thomasdufour.autodiff.Exported( $t : _root_.fr.thomasdufour.autodiff.Diff[$A] )"
        )
    }
  }
}
