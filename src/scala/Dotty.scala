//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import org.eclipse.lsp4j._
import scaled._
import scaled.util.{Close, Filler}

object Dotty {
  import spray.json._

  val ProjectFile = ".dotty-ide.json"

  case class DottyModule (
    id :String,
    compilerVersion :String,
    compilerArguments :Seq[String],
    sourceDirectories :Seq[String],
    dependencyClasspath :Seq[String],
    classDirectory :String
  )

  object DottyIDEProtocol extends DefaultJsonProtocol {
    implicit def scaledSeqFormat[T :JsonFormat] = new RootJsonFormat[Seq[T]] {
      def write(seq: Seq[T]) = JsArray(seq.map(_.toJson).toScala.toVector)
      def read(value: JsValue): Seq[T] = value match {
        case JsArray(elements) => Seq() ++ Iterable.view(elements.map(_.convertTo[T]))
        case x => deserializationError("Expected List as JsArray, but got " + x)
      }
    }
    implicit val dottyModuleFormat = jsonFormat6(DottyModule)
  }

  def parseDottyConfig (path :Path) :Seq[DottyModule] = {
    import DottyIDEProtocol._
    val bytes = Files.readAllBytes(path)
    Seq.from(JsonParser(ParserInput(bytes)).convertTo[Array[DottyModule]])
  }

  @Plugin(tag="project-root")
  class DottyRootPlugin extends RootPlugin.File(ProjectFile)

  @Plugin(tag="langserver")
  class DottyLangPlugin extends LangPlugin {
    def suffs (root :Path) = Set("scala")
    def canActivate (root :Path) = Files.exists(root.resolve(ProjectFile))
    def createClient (proj :Project) = Future.success(new DottyLangClient(proj, serverCmd(proj)))
  }

  private def serverCmd (project :Project) = {
    // TEMP: we hardcode the version of the dotty compiler for now and run it by getting the
    // classpath for this scala-project package; eventually we'll use our "download stuff from
    // Maven on demand" support to download the appropriate artifact based on what's in the
    // dotty-ide.json file and run that version of the compiler
    val pkgSvc = project.metaSvc.service[PackageService]
    val pkgSource = "git:https://github.com/scaled/scala-mode.git"
    val pkgCP = pkgSvc.classpath(pkgSource).mkString(System.getProperty("path.separator"))
    val langMain = "dotty.tools.languageserver.Main"
    Seq("java", "-classpath", pkgCP, langMain, "-stdio")
  }

  @Plugin(tag="project-resolver")
  class DottyResolverPlugin extends ResolverPlugin {
    override def addComponents (project :Project) {
      val rootPath = project.root.path
      val configFile = rootPath.resolve(ProjectFile)
      if (Files.exists(configFile)) {
        val modules = parseDottyConfig(configFile)
        val main = modules(0) // TODO: handle test project also

        val sourceDirs = main.sourceDirectories.map(rootPath.resolve(_)).toSeq
        project.addComponent(classOf[Sources], new Sources(sourceDirs))
      }
    }
  }
}

class DottyLangClient (p :Project, cmd :Seq[String]) extends LangClient(p, cmd) {

  override def name = "Dotty"

  // TEMP: right now "docs" is just a signature, so syntax highlight it; maybe some day the Dotty
  // langserver will actually return docs in addition to the signature
  override def format (buffer :Buffer, wrapWidth :Int, text :String) =
    format(buffer, text, "source.scala")

  // do great violence to type signatures to provide a terse summary
  override def formatSig (rawSig :String) :LineV = {
    var sig = Filler.flatten(rawSig)
    def skipPastNext (sig :String, c :Char, start :Int) = {
      var brackets = 0 ; var parens = 0 ; var ii = start
      while (ii < sig.length && (brackets > 0 || parens > 0 || sig.charAt(ii) != c)) {
        val c = sig.charAt(ii)
        if (c == '[') brackets += 1
        if (c == '(') parens += 1
        if (c == ']') brackets -= 1
        if (c == ')') parens -= 1
        ii += 1
      }
      ii + 1
    }
    // strip off the type parameters
    if (sig.charAt(0) == '[') sig = sig.substring(skipPastNext(sig, ']', 1))
    // strip off the implicit argument list
    val impstart = sig.indexOf("(implicit")
    if (impstart >= 0) {
      val impend = skipPastNext(sig, ')', impstart+1)
      sig = sig.substring(0, impstart) + sig.substring(impend)
    }
    // strip off qualifiers from types
    def stripQuals (sig :String) :String = {
      val stripped = sig.replaceAll("""\w+\.""", "")
      if (stripped == sig) sig
      else stripQuals(stripped)
    }
    Line(stripQuals(sig))
  }
}
