import sbt._
import sbt.Keys._

object Scalac {
  val options: Seq[String] = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint:-unused",
    "-Yno-adapted-args",
    "-Ypartial-unification",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:imports,patvars,implicits,params",
    "-Ywarn-value-discard"
  )

  def workaroundForIntellij( opts: Seq[String] ): Seq[String] =
    if (sys.props.contains( "idea.runid" ))
      forTest( opts )
    else
      opts

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
      scalacOptions                         ++= workaroundForIntellij( options ),
      scalacOptions   in Test               ~=  forTest,
      scalacOptions   in (Compile, console) ~=  forConsole,
      scalacOptions   in (Test,    console) :=  forTest( (scalacOptions in (Compile, console)).value )
    )
  // format: on
}
