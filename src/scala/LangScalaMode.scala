//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.project.LangClient
import scaled.util.Chars

@Minor(name="lang-scala",
       tags=Array("scala"), stateTypes=Array(classOf[LangClient]),
       desc="Enhances Scala mode with Language Server info.")
class LangScalaMode (env :Env) extends MinorMode(env) {
  val langClient = LangClient(buffer)

  override def keymap = super.keymap.
    bind("lang-import-type", "C-c C-i");

  @Fn("Queries for a type (completed by the language server) and adds an import for it.")
  def langImportType () {
    window.mini.read("Type:", wordAt(view.point()), wspace.historyRing("lang-type"),
                     langClient.symbolCompleter(window)).onSuccess(sym => {
      val fqName = sym.getContainerName + "." + sym.getName
      ScalaCode.insertImport(buffer, fqName)
    });
  }

  /** Returns the "word" at the specified location in the buffer. */
  private def wordAt (loc :Loc) = buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
