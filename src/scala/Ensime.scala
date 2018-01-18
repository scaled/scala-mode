//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import com.google.common.cache.LoadingCache
import java.nio.file.{Files, Path, Paths}
import scaled._
import scaled.pacman.JDK
import scaled.util.Close

object EnsimeConfig {

  val DotEnsime = ".ensime"

  // SExpr AST and parser, because Emacs
  sealed trait SExp {
    def asString :Option[String] = None
    def asList :Seq[SExp] = Seq()
    def asMap :SMap = Map()
  }
  type SMap = Map[String,SExp]
  case class SList (elems :Seq[SExp]) extends SExp {
    override def asList = elems
    override def asMap = Map() ++ elems.grouped(2).flatMap {
      case Seq(SAtom(key), value) => Some(key -> value)
      case group => println(s"Invalid sexp-map pair: $group") ; None
    }
    override def toString = elems.mkString("(", ",", ")")
  }
  case class SString (value :String) extends SExp {
    override def asString = Some(value)
    override def toString = '"' + value + '"'
  }
  case class SAtom (value :String) extends SExp {
    override def toString = value
  }
  case object SEnd extends SExp // used during parsing

  def parseSExp (path :Path) :SExp = {
    val in = Files.newBufferedReader(path)

    // hacky way to allow parseSExp to back up when it sees ')'
    var unread = 0.toChar
    def read :Char = unread match {
      case 0 => in.read match {
        case -1 => throw new IllegalStateException(s"Unexpected EOF")
        case ic => ic.toChar
      }
      case c => try c finally unread = 0.toChar
    }

    def parseSList (accum :Seq.Builder[SExp]) :SList = parseSExp match {
      case SEnd => SList(accum.build)
      case sexp => accum += sexp ; parseSList(accum)
    }

    def parseSString :SString = {
      val buffer = new java.lang.StringBuilder
      var escape = false ; var done = false
      do {
        val c = read
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
        val c = read
        if (c == ')') { unread = c ; done = true } // oops, back up!
        else if (c == ' ' || c == '\n') done = true
        else buffer.append(c)
      } while (!done)
      SAtom(buffer.toString)
    }

    def parseComment :SExp = in.read.toChar match {
      case '\n' => parseSExp
      case -1   => SAtom("")
      case _    => parseComment
    }

    def parseSExp :SExp = read match {
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

  abstract class DataConfig (val data :SMap) {
    def list (key :String) = data.get(key).map(_.asList) || Seq()
    def string (key :String) = data.get(key).flatMap(_.asString)
    def strings (key :String) = list(key).flatMap(_.asString)
    def paths (key :String) = strings(key).map(dir => Paths.get(dir))
  }

  class EnsimeId (data :SMap) extends DataConfig(data) {
    def project = string(":project") || "unknown"
    def config = string(":config") || "unknown"
    def module :String = s"$project:$config"
    def testModule :Option[String] = if (config == "compile") Some(s"$project:test") else None
  }
  class EnsimeProject (data :SMap) extends DataConfig(data) {
    val id = new EnsimeId(data.get(":id").map(_.asMap) || Map())
    def name = if (id.config == "compile") id.project else s"${id.project}-${id.config}"
  }

  class Config (path :Path, data :SMap) extends DataConfig(data) {
    if (data.isEmpty) println(s"$path does not appear to contain sexp-map data?")
    val projects = list(":projects").map(pd => new EnsimeProject(pd.asMap))
  }

  /** A cache from `Path` to `Config` for `.ensime` files. */
  val configCache :LoadingCache[Path,Config] =
    Mutable.cacheMap(path => new Config(path, parseSExp(path).asMap))

  def main (args :Array[String]) {
    Seq.from(args) flatMap(path => configCache.get(Paths.get(path)).projects) foreach { proj =>
      println(proj.name)
      proj.data foreach { ent => println(s" ${ent._1} $${ent._2}") }
    }
  }
}

object Ensime {
  import EnsimeConfig._

  @Plugin(tag="project-root")
  class EnsimeRootPlugin extends RootPlugin.File(EnsimeConfig.DotEnsime) {
    override protected def createRoot (paths :List[Path], path :Path) = {
      val sentinel = paths.head
      val configFile = path.resolve(DotEnsime)
      val config = configCache.get(configFile)
      // find the project or subproject that contains the sentinel file
      config.projects.find(_.paths(":sources").exists(sentinel startsWith _)) match {
        case Some(proj) => Project.Root(path, proj.id.module)
        case          _ => Project.Root(path)
      }
    }
  }

  class EnsimeDepends (project :Project, enproj :EnsimeProject) extends Depends(project) {
    import Project._

    val rootPath = project.root.path
    val moduleDeps = enproj.list(":depends").map(d => new EnsimeId(d.asMap))

    // if we have no other depends (we're a leave module) add an implicit depend on the JDK
    // (TODO: would be nice to know which version to use; also implicit dep on scala-library?)
    lazy val ids = if (moduleDeps.isEmpty) Seq(PlatformId(JavaPlatform, JDK.thisJDK.majorVersion))
    else {
      val seen = new java.util.HashSet[Id]()
      val depIds = Seq.builder[Id]()
      def add (id :Id) :Unit = if (seen.add(id)) depIds += id
      for (id <- moduleDeps) add(RootId(rootPath, id.module))
      for (id <- moduleDeps ;
           depId <- project.pspace.projectFor(Root(rootPath, id.module)).depends.ids) add(depId)
      depIds.build()
    }

    def depProjs :Seq[Project] = ids.flatMap(id => project.pspace.projectFor(id))

    def compileDeps = enproj.paths(":targets") ++ enproj.paths(":library-jars")
    def runtimeDeps = Seq() // TODO: use runtime-deps from :subprojects?

    // TODO: we should maybe filter out repeats?
    def buildClasspath :Seq[Path] = compileDeps ++ depProjs.flatMap(_.depends match {
      case endeps :EnsimeDepends => endeps.compileDeps
      case _                     => Seq()
    })
    def execClasspath :Seq[Path] = runtimeDeps ++ buildClasspath
  }

  @Plugin(tag="project-resolver")
  class EnsimeResolverPlugin extends ResolverPlugin {
    override def metaFiles (root :Project.Root) = Seq(root.path.resolve(DotEnsime))
    override def readdComponents (project :Project) {
      configCache.invalidate(project.root.path.resolve(DotEnsime))
      addComponents(project)
    }
    override def addComponents (project :Project) {
      val rootPath = project.root.path
      val encfg = configCache.get(rootPath.resolve(DotEnsime))
      val enproj = encfg.projects.find(_.id.module == project.root.module) || encfg.projects.head

      // add a filer component with custom ignores
      val igns = Ignorer.stockIgnores
      // ignore the SBT project build directories
      val projectDir = rootPath.resolve("project")
      igns += Ignorer.ignorePath(projectDir.resolve("target"), rootPath)
      igns += Ignorer.ignorePath(projectDir.resolve("project"), rootPath)
      // ignore the target directories for *all* modules (since we all share the same root)
      encfg.projects.foreach { proj =>
        proj.paths(":targets") foreach { target => igns += Ignorer.ignorePath(target, rootPath) }
      }
      project.addComponent(classOf[Filer], new DirectoryFiler(project, igns))

      project.addComponent(classOf[Sources], new Sources(enproj.paths(":sources")))

      val depends = new EnsimeDepends(project, enproj)
      project.addComponent(classOf[Depends], depends)

      // a hack to find the 'target' directory, given an SBT classes directory like
      // target/scala-2.12/classes
      def findTarget (path :Path, orig :Path) :Path =
        if (path == null) orig
        else if (path.getFileName.toString == "target") path
        else findTarget(path.getParent, orig)

      val java = new JavaComponent(project);
      val targets = enproj.paths(":targets")
      java.javaMetaV() = new JavaMeta(
        targets,
        findTarget(targets.head, targets.head),
        targets.head,
        depends.buildClasspath,
        depends.execClasspath
      )
      project.addComponent(classOf[JavaComponent], java)
      java.addTesters()

      val scalaVers = encfg.string(":scala-version") || "2.12.4"
      project.addComponent(classOf[Compiler], new ScalaCompiler(project, java) {
        override def javacOpts = enproj.strings(":javac-options")
        override def scalacOpts = enproj.strings(":scalac-options").
          // filter out this arg that ensime-sbt helpfully adds;
          // it causes scalac to freak out about macros not being expanded
          filter(_ != "-Ymacro-expand:discard")
        override def scalacVers = scalaVers
        // override protected def willCompile () = copyResources()
      })

      val oldMeta = project.metaV()
      val testRoot = enproj.id.testModule.map(Project.Root(rootPath, _))
      project.metaV() = oldMeta.copy(name = enproj.name, testRoot=testRoot)
    }
  }

  // TEMP: disabled until Ensime lang server is less "experimental"
  // @Plugin(tag="langserver")
  class EnsimeLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("scala")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(DotEnsime))
    def createClient (metaSvc :MetaService, root :Project.Root) =
      Future.success(new EnsimeLangClient(metaSvc, root.path))
  }

  def serverCmd (root :Path) = {
    val config = configCache.get(root.resolve(DotEnsime))

    val compilerJars = config.strings(":scala-compiler-jars")
    val serverJars = config.strings(":ensime-server-jars")
    val pathSep = System.getProperty("path.separator")
    val classpath = serverJars.mkString(pathSep) + pathSep + compilerJars.mkString(pathSep)

    Seq("java",
        "-classpath", classpath,
        "-Dlsp.workspace=" + root,
        // "-Dlsp.logLevel=" + logLevel,
        "org.ensime.server.Server", "--lsp")
  }
}

class EnsimeLangClient (msvc :MetaService, root :Path)
    extends LangClient(msvc, root, Ensime.serverCmd(root)) {

  override def name = "Ensime"
}
