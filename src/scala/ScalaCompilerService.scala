//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

/** [[ScalaCompilerService]] statics. */
object ScalaCompilerService {

  /** Encapsulates a compilation request.
    * @param id a unique identifier used to distinguish the calling project's series of compilation
    * requests from those of other projects.
    * @param buffer the buffer into which compiler output will be sent.
    * @param classpath the classpath to use during compilation.
    * @param target a directory into which temporary build files can be written.
    * @param output the directory into which to write the compiled classfiles, usually
    * `${target}/classes}`.
    * @param jcopts options to pass to javac.
    * @param scopts options to pass to scalac.
    * @param scvers the version of scalac to use (i.e. `2.11.7`).
    * @param incremental whether to do an incremental or full compile.
    * @param sources source files or directories (which will be recursively searched for source
    * files) to be compiled.
    */
  case class Request (id :String, buffer :Buffer,
                      classpath :SeqV[Path], target :Path, output :Path,
                      jcopts :SeqV[String], scopts :SeqV[String], scvers :String,
                      incremental :Boolean, sources :SeqV[Path])
}

@Service(name="scala-compiler", impl="ScalaCompilerManager", desc="""
  Provides a shared Scala compiler daemon for great compilations.""")
trait ScalaCompilerService {

  /** Requests that a compilation be initiated with the specified parameters.
    * @return a future indicating compilation completion. True indicates a successful compile, false
    * indicates one or more errors.
    */
  def compile (request :ScalaCompilerService.Request) :Future[Boolean]

  /** Terminates the existing compiler daemon and aborts any in-progress and pending requests. The
    * next call to [[compile]] will trigger the creation of a new compiler daemon. This is useful
    * if, heaven forbid, the compiler daemon becomes hosed for Odersky-knows-what reason.
    */
  def reset () :Unit

  /** Requests the status of the compiler daemon. Returns a bunch of internal debugging info. */
  def getStatus () :Future[String]
}
