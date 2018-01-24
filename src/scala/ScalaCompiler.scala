//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.{Files, Path}
import scaled._
import scaled.util.{BufferBuilder, Chars, Errors}

object ScalaCompiler {
  // matches: "P [Severity] /foo/bar/baz.scala:NN: message" the "problem" format used by Zinc
  // (plus the P prefix added by our runner)
  val probM = Matcher.regexp("""^P \[(\S+)\] (\S+):(\d+): (.*)""")
  // matches "/foo/bar/baz.java:NN: severity: message" which is what Zinc passes through if javac
  // reports an error
  val jprobM = Matcher.regexp("""(\S+):(\d+): (\S+): (.*)""")
  // matches: "     ^"
  val caretM = Matcher.regexp("""^(\s*)\^""")

  /** The default version of scalac used if none is specified. */
  val DefaultScalacVersion = "2.12.4"
}

abstract class ScalaCompiler (proj :Project, java :JavaComponent) extends Compiler(proj) {
  import Compiler._
  import ScalaCompiler._

  /** The build classpath (needed by ScalaExtractor). */
  def buildClasspath :SeqV[Path] = java.buildClasspath

  /** Options to pass to `javac`. */
  def javacOpts :SeqV[String] = Seq()
  /** Options to pass to `scalac`. */
  def scalacOpts :SeqV[String] = Seq()
  /** The version of the Scala compiler to use. */
  def scalacVers :String = DefaultScalacVersion

  val log = proj.metaSvc.log
  val compileSvc = proj.metaSvc.service[ScalaCompilerService]

  override def reset () :Unit = compileSvc.reset()

  override def describeEngine = "zinc + scalac"

  override def describeOptions (bb :BufferBuilder) {
    bb.addKeyValue("javac: ", if (javacOpts.isEmpty) "<none>" else javacOpts.mkString(" "))
    bb.addKeyValue("scalac: ", if (scalacOpts.isEmpty) "<none>" else scalacOpts.mkString(" "))
    bb.addKeyValue("scvers: ", scalacVers)
  }

  override def getStatus (buffer :Buffer) = compileSvc.getStatus.
    onSuccess(status => buffer.append(Line.fromTextNL(status))).
    onFailure(ex => buffer.append(Errors.stackTraceToLines(ex)))

  protected def compile (buffer :Buffer, file :Option[Path]) =
    compile(buffer, file, proj.sources.dirs, java.buildClasspath, java.targetDir, java.outputDir)

  /** A hook called just before we initiate compilation. */
  protected def willCompile () {}

  protected def compile (buffer :Buffer, file :Option[Path], sources :SeqV[Path],
                         classpath :SeqV[Path], target :Path, output :Path) = {
    willCompile()
    compileSvc.compile(ScalaCompilerService.Request(project.root.toString, buffer,
                                                    classpath, target, output,
                                                    javacOpts, scalacOpts, scalacVers,
                                                    file.isDefined, sources))
  }

  override def nextNote (buffer :Buffer, start :Loc) = {
    def extractNote (ploc :Loc, file :String, line :Int, sev :String, errPre :String) = try {
      // now search for the caret that indicates the error column
      var pnext = ploc.nextStart
      val ecol = buffer.findForward(caretM, pnext) match {
        case Loc.None => 0
        case cloc     => buffer.line(cloc).indexOf('^')
      }
      // every line after the path with leading whitespace is part of the message
      val desc = Seq.builder[String]()
      desc += errPre
      while (pnext < buffer.end && buffer.line(pnext).indexOf(Chars.isWhitespace) == 0) {
        desc += buffer.line(pnext).asString
        pnext = pnext.nextStart
      }
      NoteLoc(Note(Store(file), Loc(line-1, ecol), desc.build(), sev.toLowerCase == "error"), pnext)
    } catch {
      case e :Exception => log.log("Error parsing error buffer", e) ; NoMoreNotes
    }
    buffer.findForward(probM, start) match {
      case Loc.None => buffer.findForward(jprobM, start) match {
        case Loc.None => NoMoreNotes
        case ploc =>
          val (file, line, sev) = (jprobM.group(1), jprobM.group(2).toInt, jprobM.group(3))
          extractNote(ploc, file, line, sev, probM.group(4).trim)
      }
      case ploc =>
        val (sev, file, line) = (probM.group(1), probM.group(2), probM.group(3).toInt)
        extractNote(ploc, file, line, sev, probM.group(4).trim)
    }
  }
}
