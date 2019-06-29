package fr.thomasdufour.autodiff

package object examples {
  {
    case class Address( street: String, city: String )
    case class Person( name: String, age: Int, address: Address )

    import fr.thomasdufour.autodiff._

    implicit val personDiff: Diff[Person] = {
      import derived.auto._
      derived.semi.diff
    }

    Pretty.colorized2.showDiff(
      Person( "Jean Martin", 29, Address( "2 rue Pasteur", "Lille" ) ),
      Person( "Jean Martin", 55, Address( "2 rue Pasteur", "Lyon" ) )
    )
  }
}
