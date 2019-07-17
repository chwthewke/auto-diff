import sbt._
import sbt.Keys._

object Scalac {
  def makeOptions( scalaVersion: String ): Seq[String] =
    Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:higherKinds",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:-unused",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:imports,patvars,implicits,params",
      "-Ywarn-value-discard"
    ) ++ extraOptions( scalaVersion )

  def extraOptions( scalaVersion: String ): Seq[String] =
    CrossVersion.partialVersion( scalaVersion ) match {
      case Some( ( 2, y ) ) if y <= 12 =>
        Seq( "-Xfuture", "-Yno-adapted-args", "-Ypartial-unification" )
      case _ =>
        Nil
    }

  def forTest( opts: Seq[String] ): Seq[String] =
    opts.filterNot( _ == "-Ywarn-value-discard" )

  def forConsole( opts: Seq[String] ): Seq[String] =
    opts
      .filterNot( _ == "-Xfatal-warnings" )
      .filterNot( _.startsWith( "-Ywarn-unused" ) )
      .filterNot( _.startsWith( "-Xlint" ) )

  val settings: Seq[Def.Setting[_]] =
    // format: off
    Seq(
      scalacOptions                         ++= makeOptions( scalaVersion.value ),
      scalacOptions   in Test               ~=  forTest,
      scalacOptions   in (Compile, console) ~=  forConsole,
      scalacOptions   in (Test,    console) :=  forTest( (scalacOptions in (Compile, console)).value )
    )
  // format: on
}
