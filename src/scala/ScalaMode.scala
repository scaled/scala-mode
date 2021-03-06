//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import codex.model.Kind
import scaled._
import scaled.grammar.GrammarCodeMode
import scaled.project.Intel
import scaled.util.{Chars, Paragrapher}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala", ".*\\.sbt"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import scaled.util.Chars._
  import Syntax.{HereDocLiteral => HD}

  override def langScope = "source.scala"

  override def keymap = super.keymap.
    bind("import-type", "C-c C-i");

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter = ScalaIndenter.create(config)

  override protected def canAutoFill (p :Loc) :Boolean =
    super.canAutoFill(p) || (buffer.syntaxNear(p) == HD)

  override val commenter = new Commenter() {
    override def linePrefix  = "//"
    override def blockOpen   = "/*"
    override def blockPrefix = "*"
    override def blockClose  = "*/"
    override def docOpen     = "/**"

    override def mkParagrapher (syn :Syntax, buf :Buffer) = new DocCommentParagrapher(syn, buf)

    // the scala grammar marks all whitespace leading up to the open doc in comment style, so we
    // have to hack this predicate a bit
    override def inDocComment (buffer :BufferV, p :Loc) :Boolean = {
      super.inDocComment(buffer, p) && {
        val line = buffer.line(p)
        (line.indexOf(docOpenM, p.col) == -1)
      }
    }
  }

  @Fn("Queries for a type (completed by the analyzer) and adds an import for it.")
  def importType () :Unit = {
    val intel = Intel(buffer)
    window.mini.read("Type:", wordAt(view.point()), wspace.historyRing("lang-type"),
                     intel.symbolCompleter(Some(Kind.TYPE))).onSuccess(sym => {
      ScalaCode.insertImport(buffer, intel.fqName(sym))
    });
  }

  /** Returns the "word" at the specified location in the buffer. */
  private def wordAt (loc :Loc) = buffer.regionAt(loc, Chars.Word).map(_.asString).mkString
}
