package fr.thomasdufour.autodiff.examples

object SimpleDerivation {

  def main( args: Array[String] ): Unit = {
    case class Item( description: String )
    case class Bag( items: List[Item] )

    import fr.thomasdufour.autodiff.Diff
    import fr.thomasdufour.autodiff.Pretty
    import fr.thomasdufour.autodiff.derived

    implicit val bagDiff: Diff[Bag] = {
      import derived.auto._
      derived.semi.diff
    }

    println(
      Pretty.colorized2.showDiff(
        Bag( Item( "a wombat" ) :: Item( "coffee" ) :: Item( "a green fountain pen" ) :: Nil ),
        Bag( Item( "4 paperclips" ) :: Item( "coffee" ) :: Nil )
      )
    )
  }

}
