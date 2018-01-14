import sbt._

object Boilerplate {
  val pkgs = "fr" :: "thomasdufour" :: "autodiff" :: Nil
  val pkg  = pkgs.mkString( "." )
  val nel  = "cats.data.NonEmptyList"

  def genForTuple( n: Int ): String = {
    def diffApp( i: Int ) = s"D$i(l._$i, r._$i).map(Difference.Index($i, _))"

    val tupleType  = s"Tuple$n"
    val funcName   = s"for$tupleType"
    val typeParams = 1.to( n ).map( i => s"A$i" ).mkString( "[", ", ", "]" )
    val diffParams = 1.to( n ).map( i => s"D$i: Diff[A$i]" ).mkString( "(implicit ", ", ", ")" )
    val resultType = 1.to( n ).map( i => s"A$i" ).mkString( s"Diff[$tupleType[", ", ", "]]" )
    val showApps   = 1.to( n ).map( i => s"D$i.show(v._$i)" ).mkString( "List(", ", ", ")" )
    val diffApps   = 1.to( n ).map( diffApp ).mkString( "List(", ", ", ")" )

    val body =
      s"""    Diff.instance(
         |      (l, r) => 
         |        $nel
         |          .fromList($diffApps.flatten)
         |          .map(Difference.Tuple("$tupleType", _)),
         |      v => $showApps.mkString("(", ", ", ")")
         |    )
         |""".stripMargin

    s"""  implicit def $funcName$typeParams$diffParams: $resultType =
       |$body
       |""".stripMargin
  }

  def genForAllTuples: String = {
    s"""package $pkg
       |
       |trait TupleDiff {
       |
       |${1.to( 22 ).map( genForTuple ).mkString}
       |}
     """.stripMargin
  }

  // FIXME forProduct's Diff args should be Lazy
  def genForProduct( n: Int ): String = {
    val funcName    = s"forProduct$n"
    val typeParams  = 1.to( n ).map( i => s"A$i" ).mkString( "[A, ", ", ", "]" )
    val fieldParams = 1.to( n ).map( i => s"field$i: String" ).mkString( "(", ", ", ")" )
    val diffParams  = 1.to( n ).map( i => s"D$i: _root_.shapeless.Lazy[Diff[A$i]]" ).mkString( "(implicit ", ", ", ")" )

    val extractType =
      if (n == 1) "A => A1"
      else
        1.to( n ).map( i => s"A$i" ).mkString( s"A => Product$n[", ", ", "]" )

    def extractTuple( varName: String ) =
      1.to( n ).map( i => s"$varName$i" ).mkString( s"val (", ", ", s") = f($varName)" )

    val diffParts =
      1.to( n )
        .map( i => s"D$i.value(l$i,r$i).map(Difference.Field(field$i, _))" )
        .mkString( "List(", ", ", ").flatten" )

    val showParts =
      1.to( n )
        .map( i => s"""s"$$field$i = D$i.show(v$i)"""" )
        .mkString( "List(", ", ", """).mkString( s"$name(", ", ", ")")""" )

    val body =
      s"""    Diff.instance(
         |      (l, r) => {
         |        ${extractTuple( "l" )}
         |        ${extractTuple( "r" )}
         |        $nel
         |          .fromList($diffParts)
         |          .map(Difference.Product(name, _))
         |      },
         |      v => {
         |        ${extractTuple( "v" )}
         |        $showParts
         |      }
         |    )
         |""".stripMargin

    s"""  def $funcName$typeParams(name: String)$fieldParams(f: $extractType)$diffParams: Diff[A] =
       |$body
       |""".stripMargin
  }

  def genForAllProducts: String = {
    s"""package $pkg
       |
       |trait ProductDiff {
       |
       |${1.to( 22 ).map( genForProduct ).mkString}
       |}
     """.stripMargin
  }

  def gen( dir: File ): Seq[File] =
    Map( "TupleDiff.scala" -> genForAllTuples, "ProductDiff.scala" -> genForAllProducts ).map {
      case ( file, contents ) =>
        val tgt = pkgs.foldLeft( dir )( _ / _ ) / file
        IO.write( tgt, contents )
        tgt
    }.toSeq

}
