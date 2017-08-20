import sbt._
import sbt.Keys._

object Scalac {
  val options = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ypartial-unification",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  )

  // format: off
  val settings = Seq(
    scalacOptions                       ++= options,
    scalacOptions in Test               ~=  (_.filterNot(_ == "-Ywarn-valueDiscard")),
    // Note: uncomment this when importing to IntelliJ IDEA
    // scalacOptions                       ~=  (_.filterNot(_ == "-Ywarn-valueDiscard")),
    scalacOptions in (Compile, console) ~=  (_.filterNot(_ == "-Xlint").filterNot(_ == "-Ywarn-unused-import")),
    scalacOptions in (Test,    console) :=  (scalacOptions in (Compile, console)).value
  )
  // format: on
}
