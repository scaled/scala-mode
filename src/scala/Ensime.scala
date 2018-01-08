//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path, Paths}
import scaled._
import scaled.pacman.JDK
import scaled.util.Close

object EnsimeConfig {

  val DotEnsime = ".ensime"

  // SExpr parser, because Emacs
  sealed trait SExp
  case class SList (elems :Seq[SExp]) extends SExp
  case class SString (value :String) extends SExp
  case class SAtom (value :String) extends SExp
  case object SEnd extends SExp // used during parsing
  type SMap = Map[String,SExp]

  def parseSExp (path :Path) :SExp = {
    val in = Files.newBufferedReader(path)

    def parseSList (accum :Seq.Builder[SExp]) :SList = parseSExp match {
      case SEnd => SList(accum.build)
      case sexp => accum += sexp ; parseSList(accum)
    }

    def parseSString :SString = {
      val buffer = new java.lang.StringBuilder
      var escape = false ; var done = false
      do {
        val c = in.read.toChar
        if (escape) buffer.append(c)
        else if (c == '\\') escape = true
        else if (c == '"') done = true
        else buffer.append(c)
      } while (!done)
      SString(buffer.toString)
    }

    def parseSAtom (first :Char) = {
      val buffer = new java.lang.StringBuilder
      buffer.append(first)
      var done = false
      do {
        val i = in.read ; val c = i.toChar
        if (c == ' ' || c == '\n' || i < 0) done = true
        else buffer.append(c)
      } while (!done)
      SAtom(buffer.toString)
    }

    def parseComment :SExp = in.read.toChar match {
      case '\n' => parseSExp
      case -1   => SAtom("")
      case _    => parseComment
    }

    def parseSExp :SExp = in.read.toChar match {
      case ';'      => parseComment
      case '('      => parseSList(Seq.builder())
      case ')'      => SEnd
      case '"'      => parseSString
      case ' '|'\n' => parseSExp
      case c        => parseSAtom(c)
    }

    try parseSExp
    finally in.close()
  }

  def getString (sexp :SExp) :Option[String] = sexp match {
    case SString(value) => Some(value)
    case _ => None
  }

  def getList (sexp :Option[SExp]) :Seq[SExp] = sexp match {
    case Some(SList(exps)) => exps
    case _ => Seq()
  }

  def toMap (sexp :SExp) :SMap = sexp match {
    case SList(exps) => Map() ++ exps.grouped(2).flatMap {
      case Seq(SAtom(key), value) => Some(key -> value)
      case group => println(s"Invalid sexp-map pair: $group") ; None
    }
    case sexp => Map()
  }

  def parseConfig (path :Path) :SMap = {
    val map = toMap(parseSExp(path))
    if (map.isEmpty) println(s"$path does not appear to contain sexp-map data?")
    map
  }

  def main (args :Array[String]) {
    args foreach { path => println(parseConfig(Paths.get(args(0)))) }
  }
}

object Ensime {
  import EnsimeConfig._

  @Plugin(tag="project-root")
  class EnsimeRootPlugin extends RootPlugin.File(EnsimeConfig.DotEnsime)

  @Plugin(tag="project-resolver")
  class EnsimeResolverPlugin extends ResolverPlugin {
    override def addComponents (project :Project) {
      val configFile = project.root.path.resolve(DotEnsime)
      if (Files.exists(configFile)) {
        val config = parseConfig(configFile)
        val scalaVers = config.get(":scala-version").flatMap(getString) || "2.12.0"

        // TODO: handle more than just main project
        val projs = getList(config.get(":projects")).map(toMap)
        val main = projs.head

        def getStrings (key :String) = getList(main.get(key)).flatMap(getString)
        def getPaths (key :String) = getStrings(key).map(dir => Paths.get(dir))
        val sourceDirs = getPaths(":sources")
        project.addComponent(classOf[Sources], new Sources(sourceDirs))

        val java = new JavaComponent(project);
        val targets = getPaths(":targets")
        java.javaMetaV() = new JavaMeta(
          targets,
          targets.head,
          targets ++ getPaths(":library-jars"),
          targets ++ getPaths(":library-jars")
        )
        project.addComponent(classOf[JavaComponent], java)

        project.addComponent(classOf[Compiler], new ScalaCompiler(project, java) {
          override def javacOpts = getStrings(":javac-options")
          override def scalacOpts = getStrings(":scalac-options")
          override def scalacVers = scalaVers
          // override protected def willCompile () = copyResources()
        })

        // if the project has no depends, add a simple depends with the JDK, better than nothing
        if (!project.hasComponent(classOf[Depends])) {
          project.addComponent(classOf[Depends], new Depends(project) {
            import Project._
            override def ids = Seq(PlatformId(JavaPlatform, JDK.thisJDK.majorVersion))
          })
        }

        val name = config.get(":name").flatMap(getString) || "<missing name>";
        val oldMeta = project.metaV()
        project.metaV() = oldMeta.copy(name = name)
      }
    }
  }

  // TEMP: disabled until Ensime lang server is less "experimental"
  // @Plugin(tag="langserver")
  class EnsimeLangPlugin extends LangPlugin {
    def suffs (root :Path) = Set("scala")
    def canActivate (root :Path) = Files.exists(root.resolve(DotEnsime))
    def createClient (proj :Project) = Future.success(new EnsimeLangClient(proj, serverCmd(proj)))
  }

  private def serverCmd (project :Project) = {
    val rootPath = project.root.path
    val config = parseConfig(rootPath.resolve(DotEnsime))

    val compilerJars = getList(config.get(":scala-compiler-jars")).flatMap(getString)
    val serverJars = getList(config.get(":ensime-server-jars")).flatMap(getString)
    val pathSep = System.getProperty("path.separator")
    val classpath = serverJars.mkString(pathSep) + pathSep + compilerJars.mkString(pathSep)

    Seq("java",
        "-classpath", classpath,
        "-Dlsp.workspace=" + rootPath,
        // "-Dlsp.logLevel=" + logLevel,
        "org.ensime.server.Server", "--lsp")
  }
}

class EnsimeLangClient (p :Project, cmd :Seq[String]) extends LangClient(p, cmd) {

  override def name = "Ensime"
}
