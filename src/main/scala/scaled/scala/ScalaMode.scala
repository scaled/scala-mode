//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.code.{CodeConfig, Commenter, Indenter}
import scaled.grammar.{Grammar, GrammarConfig, GrammarCodeMode}
import scaled.java.JavaIndenter

object ScalaConfig extends Config.Defs {
  import CodeConfig._
  import GrammarConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", invalidStyle),
    effacer("keyword", keywordStyle),
    effacer("string", stringStyle),

    effacer("entity.name.package", moduleStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.primitive", typeStyle),

    effacer("variable.package", moduleStyle),
    effacer("variable.import", typeStyle),
    effacer("variable.language", constantStyle),
    // effacer("variable.parameter", variableStyle), // leave params white
    effacer("variable.other.type", variableStyle)
  )

  // map TextMate grammar scopes to Scaled syntax definitions
  val syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment),
    syntaxer("comment.block", Syntax.DocComment),
    syntaxer("constant", Syntax.OtherLiteral),
    syntaxer("string", Syntax.StringLiteral)
  )

  val grammars = reloadable(Seq("Scala.ndf"))(Grammar.parseNDFs)
}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import scaled.util.Chars._

  override def configDefs = ScalaConfig :: super.configDefs

  override def keymap = super.keymap.
    bind("ENTER",   "electric-newline").
    bind("S-ENTER", "electric-newline");

  override def grammars = ScalaConfig.grammars.get
  override def effacers = ScalaConfig.effacers
  override def syntaxers = ScalaConfig.syntaxers

  override val indenters = List(
    new Indenter.PairAnchorAlign(indentCtx) {
      protected val anchorM = Matcher.regexp("\\bfor\\b")
      protected val secondM = Matcher.regexp("yield\\b")
    },
    new Indenter.PairAnchorAlign(indentCtx) {
      protected val anchorM = Matcher.regexp("\\bextends\\b")
      protected val secondM = Matcher.regexp("with\\b")
    },
    new Indenter.TryCatchAlign(indentCtx),
    new Indenter.TryFinallyAlign(indentCtx),
    new Indenter.IfElseIfElseAlign(indentCtx),
    new ScalaIndenter.ContinuedExpr(indentCtx),
    new ScalaIndenter.Extends(indentCtx),
    new JavaIndenter.Javadoc(indentCtx),
    new Indenter.OneLinerWithArgs(indentCtx, blocker, Set("if", "while", "for")),
    new Indenter.OneLinerNoArgs(indentCtx, Set("else", "do", "try", "finally")),
    new ScalaIndenter.CaseBody(indentCtx),
    new Indenter.ByBlock(indentCtx) {
      override def readBlockIndent (pos :Loc) = ScalaIndenter.readBlockIndent(indentCtx, pos)
    }
  )

  override val commenter :ScalaCommenter = new ScalaCommenter() {
    // the scala grammar marks all whitespace leading up to the open doc in comment style, so we
    // have to hack this predicate a bit
    override def inDoc (buffer :BufferV, p :Loc) :Boolean = {
      super.inDoc(buffer, p) && {
        val line = buffer.line(p)
        (line.indexOf(openDocM, p.col) == -1)
      }
    }
  }

  override def detectIndent = new Indenter.Detecter(3) {
    private val toksM = Matcher.regexp("(val|var|def|override|protected|private) ")
    // if the line starts with one of the above tokens then it is meaningful
    def consider (line :LineV, start :Int) :Int = if (line.matches(toksM, start)) 1 else 0
  }.detectIndent(buffer)

  //
  // FNs

  @Fn("""Inserts a newline, then indents the subsequent line. Handles other "smart" cases such as:
         If newline is inserted in the middle of a Scaladoc comment, the next line is prepended with
         * before indenting. TODO: other smarts.""")
  def electricNewline () {
    // shenanigans to determine whether we should auto-insert the doc prefix (* )
    val inDoc = commenter.inDoc(buffer, view.point())
    newline()
    if (inDoc) view.point() = commenter.insertDocPre(buffer, view.point())
    reindentAtPoint()
  }

  // TODO: more things!
}
