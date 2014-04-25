//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.grammar.{Grammar, GrammarCodeConfig, GrammarCodeMode}
import scaled.major.CodeConfig
import scaled.util.{Chars, Indenter}

object ScalaConfig extends Config.Defs {
  import EditorConfig._
  import CodeConfig._
  import GrammarCodeConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val effacers = List(
    effacer("comment.line", commentStyle),
    effacer("comment.block", docStyle),
    effacer("constant", constantStyle),
    effacer("invalid", warnStyle),
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

  lazy val grammars = Seq(Grammar.parse(stream("Scala.tmLanguage")))
}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (env :Env) extends GrammarCodeMode(env) {
  import CodeConfig._
  import Chars._

  override def configDefs = ScalaConfig :: super.configDefs

  override def keymap = super.keymap ++ Seq(
    "ENTER"   -> "electric-newline",
    "S-ENTER" -> "electric-newline"
  )

  override protected def grammars = ScalaConfig.grammars
  override protected def effacers = ScalaConfig.effacers

  override protected def createIndenters () = List(
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
    new ScalaIndenter.Scaladoc(config, buffer),
    new Indenter.OneLinerWithArgs(config, buffer, blocker, Set("if", "while", "for")),
    new Indenter.OneLinerNoArgs(config, buffer, Set("else", "do", "try", "finally")),
    new ScalaIndenter.CaseBody(config, buffer),
    new Indenter.ByBlock(config, buffer) {
      override def readBlockIndent (pos :Loc) = ScalaIndenter.readBlockIndent(buffer, pos)
    }
  ) ++ super.createIndenters()

  //
  // FNs

  @Fn("""Inserts a newline, then indents the subsequent line. Handles other "smart" cases such as:
         If newline is inserted in the middle of a Scaladoc comment, the next line is prepended with
         * before indenting. TODO: other smarts.""")
  def electricNewline () {
    val p = view.point()
    val onDoc = buffer.stylesAt(p) contains docStyle
    val afterDoc = (p.col > 0) && (buffer.stylesAt(p.prevC) contains docStyle)
    val afterCloseDoc = buffer.line(p).lastIndexOf(closeDocM, p.col) != -1
    val inDoc = onDoc || (afterDoc && !afterCloseDoc)
    newline()
    if (inDoc) {
      buffer.insert(view.point(), "* ", Styles.None)
      view.point() = view.point() + (0, 2)
    }
    reindentAtPoint()
  }
  private val closeDocM = Matcher.exact("*/")

  // TODO: more things!
}
