//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path, Paths}
import scaled._
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
        val c = in.read.toChar
        if (c == ' ' || c == '\n') done = true
        else buffer.append(c)
      } while (!done)
      SAtom(buffer.toString)
    }

    def parseSExp :SExp = in.read.toChar match {
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
    val map = parseConfig(Paths.get(args(0)))
    println("ensime-server-jars: " +  map.get(":ensime-server-jars"))
    println("scala-compiler-jars: " +  map.get(":scala-compiler-jars"))
    println("name: " +  map.get(":name"))
  }
}

object EnsimeLangClient {
  import EnsimeConfig._

  @Plugin(tag="langserver")
  class EnsimeLangPlugin extends LangPlugin {
    def suffs (root :Path) = Set("scala")
    def canActivate (root :Path) = Files.exists(root.resolve(DotEnsime))
    def createClient (proj :Project) = Future.success(new EnsimeLangClient(proj, serverCmd(proj)))
  }

  def serverCmd (project :Project) = {
    val rootPath = project.root.path
    val configFile = rootPath.resolve(DotEnsime)
    val config = parseConfig(configFile)

    val compilerJars = getList(config.get(":scala-compiler-jars")).flatMap(getString)
    val serverJars = getList(config.get(":ensime-server-jars")).flatMap(getString)
    val pathSep = System.getProperty("path.separator")
    val classpath = serverJars.mkString(pathSep) + pathSep + compilerJars.mkString(pathSep)

    val cmd = Seq("java",
        "-classpath", classpath,
        "-Dlsp.workspace=" + rootPath,
        // "-Dlsp.logLevel=" + logLevel,
        "org.ensime.server.Server", "--lsp")

    import java.nio.file.StandardOpenOption._
    Files.write(Paths.get("ensime-ls.sh"), cmd, CREATE, TRUNCATE_EXISTING, WRITE);

    cmd
  }
}

class EnsimeLangClient (p :Project, cmd :Seq[String]) extends LangClient(p, cmd) {

  override def name = "Ensime"
}

object EnsimeLangProject {

  @Plugin(tag="project-finder")
  class FinderPlugin extends ProjectFinderPlugin("langserver", true, classOf[EnsimeLangProject]) {
    def checkRoot (root :Path) :Int = if (exists(root, EnsimeConfig.DotEnsime)) 1 else -1
  }
}

class EnsimeLangProject (ps :ProjectSpace, r :Project.Root) extends Project(ps, r) {
  import EnsimeLangProject._
  import EnsimeConfig._

  override protected def computeMeta (oldMeta :Project.Meta) = try {
    addComponent(classOf[Compiler], new LangCompiler(this))

    val config = ensimeConfig.get
    val projs = getList(config.get(":projects")).map(toMap)
    val main = projs.head

    val sourceDirs = getList(main.get(":sources")).flatMap(getString).map(dir => Paths.get(dir));
    addComponent(classOf[Sources], new Sources(sourceDirs))

    val name = config.get(":name").flatMap(getString) || "<missing name>";
    Future.success(oldMeta.copy(name = name))
  } catch {
    case err :Throwable => Future.failure(err)
  }

  private[this] val ensimeConfig = new Close.Ref[SMap](toClose) {
    protected def create = parseConfig(configFile)
  }

  private def rootPath = root.path
  private def configFile = rootPath.resolve(DotEnsime)
}
