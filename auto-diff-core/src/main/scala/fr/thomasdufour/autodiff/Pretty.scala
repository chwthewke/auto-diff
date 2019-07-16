package fr.thomasdufour.autodiff

trait Pretty {
  def show( d: Difference ): String

  def show( d: Option[Difference] ): String = d.fold( "" )( show )

  def showDiff[A]( left: A, right: A )( implicit D: Diff[A] ): String =
    show( D( left, right ) )
}

object Pretty {
  import text._

  /**
    *  Convenience constructor
    * @param color Whether to include ANSI codes to colorize the output
    * @param indentWidth Indent width for the multi-line output, ignored when singleLine is `true`
    * @param singleLine When true, produce single-line output
    */
  def apply( color: Boolean = true, indentWidth: Int = 2, singleLine: Boolean = false ): Pretty = {
    val colorize = if (color) Colorize.RedGreen else Colorize.Plain
    if (singleLine)
      SingleLinePretty( colorize )
    else
      DetailedPretty( indentWidth, colorize )
  }

  val plain2: Pretty        = DetailedPretty( indentWidth = 2, color = Colorize.Plain )
  val colorized2: Pretty    = DetailedPretty( indentWidth = 2, color = Colorize.RedGreen )
  val plain4: Pretty        = DetailedPretty( indentWidth = 4, color = Colorize.Plain )
  val colorized4: Pretty    = DetailedPretty( indentWidth = 4, color = Colorize.RedGreen )
  val linePlain: Pretty     = SingleLinePretty( color = Colorize.Plain )
  val lineColorized: Pretty = SingleLinePretty( color = Colorize.RedGreen )
}
