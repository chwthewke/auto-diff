package fr.thomasdufour.autodiff

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.Generic
import shapeless.HList
import shapeless.IsTuple
import shapeless.Nat

class ShapelessProductSpec
    extends WordSpec
    with Matchers
    with TypeCheckedTripleEquals
    with ScalaCheckDrivenPropertyChecks
    with ShapelessTuples {

  import DiffOps._

  class DiffTupleAsProductProp[T <: Product]( diff: Diff[T] ) {
    def check[R <: HList, N <: Nat, D]( name: String )(
        implicit genDiff: GenDiff[T],
        isTuple: IsTuple[T],
        gen: Generic.Aux[T, R],
        s: Length.Aux[R, N],
        n: ToInt[N]
    ): Unit =
      checkDiff[T, R, N, T.Field](
        diff,
        ( ix, v1, v2 ) => T.Field( s"_$ix", v1.toString !== v2.toString ),
        ds => F( name, ds ),
        t => expectedShow( name, t )
      )

    def expectedShow( name: String, t: T ): String =
      (1 to t.productArity).toVector
        .map( ix => s"_$ix = ${t.productElement( ix - 1 )}" )
        .mkString( s"$name(", ", ", ")" )
  }

  implicit val genInt: Gen[Int] = arbitrary[Int]

  def diffTupleProp[T <: Product]( diff: Diff[T] ) = new DiffTupleAsProductProp[T]( diff )

  "a Product diff for Tuple1" should {
    val diffProduct1: Diff[Tuple1[Int]] =
      Diff.forProduct1( "Tuple1" )( "_1" )( _._1 )

    behave like diffTupleProp( diffProduct1 ).check( "Tuple1" )
  }

  "a Product diff for Tuple2" should {
    val diffProduct2: Diff[( Int, Int )] =
      Diff.forProduct2( "Tuple2" )( "_1", "_2" )( x => x )

    behave like diffTupleProp( diffProduct2 ).check( "Tuple2" )
  }

  "a Product diff for Tuple3" should {
    val diffProduct3: Diff[( Int, Int, Int )] =
      Diff.forProduct3( "Tuple3" )( "_1", "_2", "_3" )( x => x )

    behave like diffTupleProp( diffProduct3 ).check( "Tuple3" )
  }

  "a Product diff for Tuple4" should {
    val diffProduct4: Diff[( Int, Int, Int, Int )] =
      Diff.forProduct4( "Tuple4" )( "_1", "_2", "_3", "_4" )( x => x )

    behave like diffTupleProp( diffProduct4 ).check( "Tuple4" )
  }

  "a Product diff for Tuple5" should {

    val diffProduct5: Diff[( Int, Int, Int, Int, Int )] =
      Diff.forProduct5( "Tuple5" )( "_1", "_2", "_3", "_4", "_5" )( x => x )

    behave like diffTupleProp( diffProduct5 ).check( "Tuple5" )
  }

  "a Product diff for Tuple6" should {

    val diffProduct6: Diff[( Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct6( "Tuple6" )( "_1", "_2", "_3", "_4", "_5", "_6" )( x => x )

    behave like diffTupleProp( diffProduct6 ).check( "Tuple6" )
  }

  "a Product diff for Tuple7" should {

    val diffProduct7: Diff[( Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct7( "Tuple7" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7" )( x => x )

    behave like diffTupleProp( diffProduct7 ).check( "Tuple7" )
  }

  "a Product diff for Tuple8" should {

    val diffProduct8: Diff[( Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct8( "Tuple8" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8" )( x => x )

    behave like diffTupleProp( diffProduct8 ).check( "Tuple8" )
  }

  "a Product diff for Tuple9" should {

    val diffProduct9: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct9( "Tuple9" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9" )( x => x )

    behave like diffTupleProp( diffProduct9 ).check( "Tuple9" )
  }

  "a Product diff for Tuple10" should {

    val diffProduct10: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct10( "Tuple10" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_10" )( x => x )

    behave like diffTupleProp( diffProduct10 ).check( "Tuple10" )
  }

  "a Product diff for Tuple11" should {

    val diffProduct11: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct11( "Tuple11" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_10", "_11" )( x => x )

    behave like diffTupleProp( diffProduct11 ).check( "Tuple11" )
  }

  "a Product diff for Tuple12" should {

    val diffProduct12: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct12( "Tuple12" )( "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9", "_10", "_11", "_12" )(
        x => x
      )

    behave like diffTupleProp( diffProduct12 ).check( "Tuple12" )
  }

  "a Product diff for Tuple13" should {

    val diffProduct13: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct13( "Tuple13" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13"
      )( x => x )

    behave like diffTupleProp( diffProduct13 ).check( "Tuple13" )
  }

  "a Product diff for Tuple14" should {

    val diffProduct14: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct14( "Tuple14" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14"
      )( x => x )

    behave like diffTupleProp( diffProduct14 ).check( "Tuple14" )
  }

  "a Product diff for Tuple15" should {

    val diffProduct15: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct15( "Tuple15" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15"
      )( x => x )

    behave like diffTupleProp( diffProduct15 ).check( "Tuple15" )
  }

  "a Product diff for Tuple16" should {

    val diffProduct16: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct16( "Tuple16" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16"
      )( x => x )

    behave like diffTupleProp( diffProduct16 ).check( "Tuple16" )
  }

  "a Product diff for Tuple17" should {

    val diffProduct17: Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct17( "Tuple17" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17"
      )( x => x )

    behave like diffTupleProp( diffProduct17 ).check( "Tuple17" )
  }

  "a Product diff for Tuple18" should {

    val diffProduct18
        : Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct18( "Tuple18" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17",
        "_18"
      )( x => x )

    behave like diffTupleProp( diffProduct18 ).check( "Tuple18" )
  }

  "a Product diff for Tuple19" should {

    val diffProduct19
        : Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct19( "Tuple19" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17",
        "_18",
        "_19"
      )( x => x )

    behave like diffTupleProp( diffProduct19 ).check( "Tuple19" )
  }

  "a Product diff for Tuple20" should {

    val diffProduct20
        : Diff[( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )] =
      Diff.forProduct20( "Tuple20" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17",
        "_18",
        "_19",
        "_20"
      )( x => x )

    behave like diffTupleProp( diffProduct20 ).check( "Tuple20" )
  }

  "a Product diff for Tuple21" should {

    val diffProduct21: Diff[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ] =
      Diff.forProduct21( "Tuple21" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17",
        "_18",
        "_19",
        "_20",
        "_21"
      )( x => x )

    behave like diffTupleProp( diffProduct21 ).check( "Tuple21" )
  }

  "a Product diff for Tuple22" should {

    val diffProduct22: Diff[
      ( Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int )
    ] =
      Diff.forProduct22( "Tuple22" )(
        "_1",
        "_2",
        "_3",
        "_4",
        "_5",
        "_6",
        "_7",
        "_8",
        "_9",
        "_10",
        "_11",
        "_12",
        "_13",
        "_14",
        "_15",
        "_16",
        "_17",
        "_18",
        "_19",
        "_20",
        "_21",
        "_22"
      )( x => x )

    behave like diffTupleProp( diffProduct22 ).check( "Tuple22" )
  }
}

object ShapelessProductSpec {

  // Note: This was used to generate the above test cases

  def strIx( i: Int ) = '"'.toString + "_" + i.toString + '"'.toString

  def showTest( n: Int ) =
    s""""a Product diff for Tuple$n" should {
       |
       |  val diffProduct$n: Diff[( ${(1 to n).map( _ => "Int" ).mkString( ", " )} )] =
       |    Diff.forProduct$n( "Tuple$n" )( ${(1 to n).map( strIx ).mkString( ", " )} )( x => x )
       |  
       |  behave like diffTupleProp( diffProduct$n ).check( "Tuple$n" )
       |}
       |""".stripMargin

}
