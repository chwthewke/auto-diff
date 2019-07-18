package fr.thomasdufour.autodiff.examples

object Header {

  def main( args: Array[String] ): Unit = {
    case class Address( street: String, city: String )
    case class Person( name: String, age: Int, address: Address )

    import fr.thomasdufour.autodiff.Diff
    import fr.thomasdufour.autodiff.Pretty
    import fr.thomasdufour.autodiff.derived

    implicit val personDiff: Diff[Person] = {
      derived.semi.diff
    }

    val difference = personDiff(
      Person( "Jean Martin", 29, Address( "2 rue Pasteur", "Lille" ) ),
      Person( "Jean Martin", 55, Address( "2 rue Pasteur", "Lyon" ) )
    )

    println( Pretty.colorized2.show( difference ) )
  }
}
