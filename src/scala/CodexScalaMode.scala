//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import codex.model.Kind
import scaled._
import scaled.project.{Codex, CodexMinorMode}

@Minor(name="codex-scala",
       tags=Array("scala"), stateTypes=Array(classOf[Codex]),
       desc="A minor mode which enhances Scala mode with Codex information.")
class CodexScalaMode (env :Env) extends CodexMinorMode(env) {

  override def keymap = super.keymap.
    bind("codex-import-type", "C-c C-i")

  @Fn("Queries for a type (completed by the project's Codex) and adds an import for it.")
  def codexImportType () :Unit = codexRead("Type:", Kind.TYPE) {
    df => ScalaCode.insertImport(buffer, df.fqName)
  }
}
