package fr.thomasdufour.autodiff
package derived

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import shapeless.Witness
import shapeless.tag.@@

class FieldDiff[K, V]( private[derived] val diff: Diff[V] )

object FieldDiff {
  def of[K] = new FieldDiffBuilder[K]

  class FieldDiffBuilder[K] {
    def apply[V]( diff: Diff[V] ): FieldDiff[K, V] = new FieldDiff( diff )
  }

  def w[V]( field: String, diff: Diff[V] )( implicit W: Witness.Aux[field.type] ): FieldDiff[Symbol @@ W.T, V] =
    new FieldDiff( diff )

  def ww[V]( field: Witness, diff: Diff[V] ): FieldDiff[Symbol @@ field.T, V] = new FieldDiff( diff )

  def m( name: String ): FieldDiffB = macro FieldDiffMacros.mkFieldDiffImpl
}

class FieldDiffB {
  type K

  def apply[V]( diff: Diff[V] ): FieldDiff[K, V] = new FieldDiff[K, V]( diff )
}

class FieldDiffMacros( val c: whitebox.Context ) {
  import c.universe._

  val SymTpe  = typeOf[scala.Symbol]
  val atatTpe = typeOf[@@[_, _]].typeConstructor

  def singletonType( n: String ): Type =
    appliedType( atatTpe, List( SymTpe, c.internal.constantType( Constant( n ) ) ) )

  def mkFieldDiffImpl( name: Tree ): Tree = {
    val nameString: String = name match {
      case Literal( Constant( s: String ) ) => s
      case _                                => c.abort( c.enclosingPosition, s"cannot match $name" )
    }

    val ftpe = singletonType( nameString )

    q"""new _root_.fr.thomasdufour.autodiff.derived.FieldDiffB { type K = $ftpe }"""
  }

}
