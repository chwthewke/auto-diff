package fr.thomasdufour.autodiff
package derived

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec

class RecursiveParametricStdSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  import RecursiveParametricStdSpec._
  import DiffOps._

  "Deriving a Diff for a recursive case class" which {

    "has recursion under an Option" should {
      val diff: Diff[OptionRec] = {
        import auto._
        semi.diff
      }

      "use the provided Option diff" in {

        diff(
          OptionRec( Some( OptionRec( None ) ) ),
          OptionRec( Some( OptionRec( Some( OptionRec( None ) ) ) ) )
        ).tree should ===(
          F(
            "OptionRec",
            "rec" -> T(
              T.Coproduct,
              "Option",
              F( "OptionRec", "rec" -> T( T.Coproduct, "Option", "None" !== "Some(OptionRec(rec: None))" ) )
            )
          )
        )

      }
    }

    "has recursion under an Either" should {
      val diff: Diff[EitherRec] = {
        import auto._
        semi.diff
      }

      "use the provided either diff" in {
        diff(
          EitherRec( Right( EitherRec( Left( "foo" ) ) ) ),
          EitherRec( Right( EitherRec( Left( "bar" ) ) ) ),
        ).tree should ===(
          F(
            "EitherRec",
            "rec" -> T( T.Coproduct, "Right", F( "EitherRec", "rec" -> T( T.Coproduct, "Left", "foo" !== "bar" ) ) )
          )
        )
      }
    }

    "has recursion under a List" should {

      val diff: Diff[ListRec] = {
        import auto._
        semi.diff
      }

      "use the provided List diff" in {

        diff(
          ListRec( ListRec( Nil ) :: ListRec( ListRec( Nil ) :: Nil ) :: Nil ),
          ListRec( ListRec( Nil ) :: ListRec( Nil ) :: Nil )
        ).tree should ===(
          F(
            "ListRec",
            "rec" -> I(
              T.Seq,
              "List",
              1 -> F( "ListRec", "rec" -> I( T.Seq, "List", 0 -> ("ListRec(rec: List())" !== "<end>") ) )
            )
          )
        )

      }
    }

    "has recursion under a Tuple" should {
      val diff: Diff[TupleRec] = {
        import auto._
        semi.diff
      }

      "use the provided Tuple Diff" in {

        diff(
          TupleRec2( TupleRec0, TupleRec2( TupleRec0 -> TupleRec0 ) ),
          TupleRec2( TupleRec0, TupleRec2( TupleRec0, TupleRec2( TupleRec0, TupleRec0 ) ) ),
        ).tree should ===(
          T(
            T.Coproduct,
            "TupleRec",
            F(
              "TupleRec2",
              "rec" -> I(
                T.Tuple,
                "Tuple2",
                2 ->
                  T(
                    T.Coproduct,
                    "TupleRec",
                    F(
                      "TupleRec2",
                      "rec" -> I(
                        T.Tuple,
                        "Tuple2",
                        2 ->
                          T( T.Coproduct, "TupleRec", "TupleRec0(...)" !== "TupleRec2(...)" )
                      )
                    )
                  )
              )
            )
          )
        )
      }
    }

  }

  // TODO: a bunch more

}

object RecursiveParametricStdSpec {
  case class OptionRec( rec: Option[OptionRec] )
  case class EitherRec( rec: Either[String, EitherRec] )
  case class ListRec( rec: List[ListRec] )

  sealed trait TupleRec
  final case object TupleRec0                               extends TupleRec
  final case class TupleRec2( rec: ( TupleRec, TupleRec ) ) extends TupleRec
  object TupleRec2 {
    def apply( x: TupleRec, y: TupleRec ): TupleRec2 = TupleRec2( ( x, y ) )
  }
}
