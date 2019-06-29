package fr.thomasdufour.autodiff
package text

abstract class Colorize extends Product with Serializable {
  def left( s: String ): String
  def right( s: String ): String
}

object Colorize {
  case object RedGreen extends Colorize {
    override def left( s: String ): String  = Console.RED + s + Console.RESET
    override def right( s: String ): String = Console.GREEN + s + Console.RESET
  }

  case object Plain extends Colorize {
    override def left( s: String ): String  = s
    override def right( s: String ): String = s
  }
}
