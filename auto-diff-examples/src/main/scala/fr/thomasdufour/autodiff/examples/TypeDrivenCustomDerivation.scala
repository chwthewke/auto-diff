package fr.thomasdufour.autodiff.examples

object TypeDrivenCustomDerivation {

  def main( args: Array[String] ): Unit = {
    case class Item( description: String )
    case class Bag( items: List[Item] )

    import fr.thomasdufour.autodiff.Diff
    import fr.thomasdufour.autodiff.Pretty
    import fr.thomasdufour.autodiff.derived

    implicit val bagDiff: Diff[Bag] = {
      import derived.auto._
      implicit val diffItems: Diff[List[Item]] = Diff.inAnyOrder

      derived.semi.diff
    }

    println(
      Pretty.colorized2.showDiff(
        Bag( Item( "a wombat" ) :: Nil ),
        Bag( Item( "4 paperclips" ) :: Item( "a wombat" ) :: Nil )
      )
    )
  }

}
