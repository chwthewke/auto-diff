import sbt._
import org.scalafmt.sbt.ScalafmtPlugin
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._

object Scalafmt extends AutoPlugin {
  override def requires: Plugins = super.requires && ScalafmtPlugin

  val scalafmtGenerateConfig: TaskKey[Unit] =
    TaskKey[Unit]( "scalafmtGenerateConfig" )

  override def buildSettings = Seq(
    scalafmtGenerateConfig := {
      IO.write(
        file( ".scalafmt.conf" ),
        """version = "2.0.0"
          |
          |style = defaultWithAlign
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
    scalafmtConfig := {
      val _ = scalafmtGenerateConfig.value
      file( ".scalafmt.conf" )
    }
  )
}
