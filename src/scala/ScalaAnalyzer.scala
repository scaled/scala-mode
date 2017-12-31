//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import java.nio.file.Path
import scaled._

class ScalaAnalyzer extends Analyzer {

  // override def elementAt (buffer :Buffer, loc :Loc) = Future.failure(new Exception("TODO"))

  // override def enclosing (buffer :Buffer, loc :Loc, kind :Model.Kind) =
  //   Future.failure(new Exception("TODO"))

  override protected def analyze (paths :Seq[Path]) = {
    Future.failure(new Exception("TODO!"))
  }
}
