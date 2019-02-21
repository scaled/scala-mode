//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import codex.extract.ScalaExtractor
import java.net.URLClassLoader
import java.nio.file.{Path, Paths}
import scaled._

@Plugin(tag="codex-extractor")
class ScalaExtractorPlugin extends ExtractorPlugin {

  override val suffs = Set("scala")

  // only return an extractor if our project has valid dependencies (it'll at least need a
  // scala-library dependency otherwise we're going to be hosed)
  override def extractor (project :Project, suff :String) =
    if (exClasspath(project).isEmpty) None else Some(new ScalaExtractor() {
      override def compilerArgs = exScalacOpts(project)
      override def classpath = exClasspath(project)
      override def log (msg :String) = project.log(msg)
    })

  private def exScalacOpts (project :Project) = scala.List() ++ (
    project.component(classOf[ScalaCompiler]) map { _.scalacOpts } getOrElse Seq()).toSeq.toScala

  private def exClasspath (project :Project) =
    (project.component(classOf[JavaComponent]) map { jp =>
      // if this project doesn't have scala-library on its classpath, we have to put it there; the
      // only project that's likely not to have scala-library on its classpath is scala-library
      // itself, but that's one we definitely want to index
      val cp = jp.classes ++ jp.buildClasspath
      if (cp.exists(_.getFileName.toString.contains("scala-library"))) cp
      else (cp ++ findScalaLibrary)
    } getOrElse Seq()).toSeq.toScala

  private def findScalaLibrary :Option[Path] = {
    val loader = classOf[scala.Option[_]].getClassLoader.asInstanceOf[URLClassLoader]
    val entries = Seq.from(loader.getURLs).map(url => Paths.get(url.toURI))
    entries find(_.getFileName.toString.contains("scala-library"))
  }
}
