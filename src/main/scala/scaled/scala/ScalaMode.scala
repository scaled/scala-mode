//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.code.{CodeConfig, Commenter, Indenter}
import scaled.grammar.{Grammar, GrammarConfig, GrammarCodeMode}
import scaled.java.{JavaCommenter, JavaIndenter}

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

    effacer("entity.name.package", constantStyle),
    effacer("entity.name.class", typeStyle),
    effacer("entity.other.inherited-class", typeStyle),
    effacer("entity.name.function", functionStyle),
    effacer("entity.name.val-declaration", variableStyle),

    effacer("storage.modifier", keywordStyle),
    effacer("storage.type.primitive", typeStyle),

    effacer("variable.package", constantStyle),
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

  lazy val grammars = Seq(Grammar.parse(stream("Scala.tmLanguage")))
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

  override def keymap = super.keymap ++ Seq(
    "ENTER"   -> "electric-newline",
    "S-ENTER" -> "electric-newline"
  )

  override def grammars = ScalaConfig.grammars
  override def effacers = ScalaConfig.effacers
  override def syntaxers = ScalaConfig.syntaxers

  override val indenters = List(
    new Indenter.PairAnchorAlign(config, buffer) {
      protected val anchorM = Matcher.regexp("\\bfor\\b")
      protected val secondM = Matcher.regexp("yield\\b")
    },
    new Indenter.PairAnchorAlign(config, buffer) {
      protected val anchorM = Matcher.regexp("\\bextends\\b")
      protected val secondM = Matcher.regexp("with\\b")
    },
    new Indenter.TryCatchAlign(config, buffer),
    new Indenter.TryFinallyAlign(config, buffer),
    new Indenter.IfElseIfElseAlign(config, buffer),
    new ScalaIndenter.ValueExprBody(config, buffer),
    new ScalaIndenter.Extends(config, buffer),
    new JavaIndenter.Javadoc(config, buffer),
    new Indenter.OneLinerWithArgs(config, buffer, blocker, Set("if", "while", "for")),
    new Indenter.OneLinerNoArgs(config, buffer, Set("else", "do", "try", "finally")),
    new ScalaIndenter.CaseBody(config, buffer),
    new Indenter.ByBlock(config, buffer) {
      override def readBlockIndent (pos :Loc) = ScalaIndenter.readBlockIndent(buffer, pos)
    }
  )

  override val commenter :JavaCommenter = new JavaCommenter() {
    // the scala grammar marks all whitespace leading up to the open doc in comment style, so we
    // have to hack this predicate a bit
    override def inDoc (buffer :BufferV, p :Loc) :Boolean = {
      super.inDoc(buffer, p) && {
        val line = buffer.line(p)
        (line.indexOf(openDocM, p.col) == -1)
      }
    }
  }

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
