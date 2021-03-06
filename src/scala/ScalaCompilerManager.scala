//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.Files
import java.util.{Map => JMap}
import scala.collection.mutable.Queue
import scaled._
import scaled.pacman.JDK
import scaled.prococol.{Session, SubProcess}
import scaled.util.{Close, Chars, Errors}

/** Maintains a single Zinc/Scala compiler daemon which is shared by all projects.
  * Vends it via [[ScalaCompilerService]].
  */
class ScalaCompilerManager (metaSvc :MetaService)
    extends AbstractService with ScalaCompilerService {
  import ScalaCompilerService._

  private val scSource = "git:https://github.com/scaled/scala-compiler.git"
  private val scMain = "scaled.zinc.Main"
  private def pkgSvc = metaSvc.service[PackageService]
  private val scDir = pkgSvc.installDir(scSource)
  private def scCP = pkgSvc.classpath(scSource).mkString(System.getProperty("path.separator"))

  private val toClose = new Close.Bag()
  private val session = new Close.Box[Session](toClose) {
    override def create = new Session(metaSvc.exec.ui, new SubProcess.Config() {
      def javaBin = (jdk :JDK) => jdk.home.resolve("bin").resolve("java")
      def binJava = {
        val thisJava = javaBin(JDK.thisJDK)
        if (Files.exists(thisJava)) thisJava.toString
        JDK.jdks.map(javaBin).find(java => Files.exists(java)) match {
          case Some(java) => java.toString
          case None => throw new Error("Unable to find 'java' binary in any JDK")
        }
      }
      override val command = Array(binJava, scMain)
      override val environment = Map("CLASSPATH" -> scCP).asJMap
      // override def debug = true
    }) {
      override def interactionEnded () :Unit = {
        super.interactionEnded()
        processNext()
      }
      override def onErrorOutput (text :String) = metaSvc.log.log(text)
    }
  }

  private val queue = Queue[(Request,Promise[Boolean])]()

  override def didStartup () :Unit = {} // nada
  override def willShutdown () :Unit = {
    toClose.close()
  }

  override def compile (req :Request) = {
    val result = Promise[Boolean]()
    if (session.get.interacting) queue += (req -> result)
    else compile(req, result)
    result
  }

  override def reset () :Unit = {
    // TODO: if there's a request in progress, I don't think this will fail it
    session.close()
    while (!queue.isEmpty) queue.dequeue._2.fail(Errors.feedback("Compiler reset."))
  }

  override def getStatus () :Future[String] = {
    val result = Promise[String]()
    session.get.interact("status", Map().asJMap, new Session.Interactor() {
      def onMessage (name :String, data :JMap[String,String]) = name match {
        case "status" => result.succeed(data.get("text")) ; true
        case _ =>
          val msg = s"Unexpected message from compiler [name=$name, data=$data]"
          result.fail(new Exception(msg))
          true
      }
    })
    result
  }

  private def processNext () :Unit = {
    if (!queue.isEmpty) {
      val (req, res) = queue.dequeue
      compile(req, res)
    }
  }

  private def compile (req :Request, res :Promise[Boolean]) :Unit = {
    def tabsep (elems :SeqV[AnyRef]) = elems.mkString("\t")
    val args = Map.builder[String,String]().
      put("pkgdir",    scDir.toString).
      put("sessionid", req.id).
      put("target",    req.target.toString).
      put("output",    req.output.toString).
      put("jcopts",    tabsep(req.jcopts)).
      put("scopts",    tabsep(req.scopts)).
      put("scvers",    req.scvers).
      put("classpath", tabsep(req.classpath)).
      put("increment", req.incremental.toString).
      put("sources",   tabsep(req.sources)).
      build()
    // TODO: other args like scala version, etc.

    session.get.interact("compile", args.asJMap, new Session.Interactor() {
      def onMessage (name :String, data :JMap[String,String]) = name match {
        case "log"     => req.buffer.append(Line.fromTextNL(data.get("msg"))) ; false
        case "compile" => res.succeed(data.get("result") == "success") ; true
        case _ =>
          val msg = s"Unexpected message from compiler [name=$name, data=$data]"
          req.buffer.append(Line.fromTextNL(msg))
          true
      }
    })
  }
}
