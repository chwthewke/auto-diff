package fr.thomasdufour.autodiff.examples

object Standard {

  def main( args: Array[String] ): Unit = {
    import fr.thomasdufour.autodiff.Diff
    import fr.thomasdufour.autodiff.Pretty

    def printDiff[A: Diff]( x: A, y: A ): Unit =
      println( Pretty.colorized2.showDiff( x, y ) )

    printDiff( 1, 2 )
    printDiff( "abc", "def" )

    printDiff( Some( "abc" ), None )
    printDiff( Left( "error" ), Right( 42 ) )
    printDiff[Either[String, Int]]( Right( 66 ), Right( 42 ) )

    printDiff( 1 :: 2 :: Nil, 1 :: 3 :: 4 :: Nil )
    printDiff( Map( "a" -> 1, "b" -> 2 ), Map( "b" -> 3, "a" -> 1 ) )
  }

}
