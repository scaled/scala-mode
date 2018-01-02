//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.grammar.GrammarCodeMode
import scaled.util.Paragrapher

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

  override def mkParagrapher (syntax :Syntax) =
    if (syntax != HD) super.mkParagrapher(syntax)
    else new Paragrapher(syntax, buffer) {
      override def isDelim (row :Int) = super.isDelim(row) || {
        val ln = line(row)
        (ln.syntaxAt(0) != HD) || (ln.syntaxAt(ln.length-1) != HD)
      }
    }

  override protected def createIndenter = new ScalaIndenter(config)

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

  // TODO: more things!
}
