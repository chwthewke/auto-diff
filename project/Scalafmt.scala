import sbt._
import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._

object Scalafmt {

  val scalafmtGenerateConfig: TaskKey[Unit] =
    TaskKey[Unit]("scalafmtGenerateConfig")

  val settings = Seq(
    scalafmtGenerateConfig := {
      IO.write(
        file(".scalafmt.conf"),
        """style = defaultWithAlign
          |maxColumn = 120
          |lineEndings = preserve
          |
          |assumeStandardLibraryStripMargin = true
          |align.arrowEnumeratorGenerator = true
          |spaces.inParentheses = true
          |
          |rewrite.rules = [ExpandImportSelectors]
          |""".stripMargin
      )
    },
    scalafmtOnCompile := true,
    scalafmtVersion := "1.1.0",
    scalafmtConfig := {
      val _ = scalafmtGenerateConfig.value
      file(".scalafmt.conf")
    }
  )
}
