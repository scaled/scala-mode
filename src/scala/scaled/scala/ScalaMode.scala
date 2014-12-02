//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.code.{CodeConfig, Commenter}
import scaled.grammar.{Grammar, GrammarConfig, GrammarCodeMode}

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

  val grammars = resource("Scala.ndf")(Grammar.parseNDFs)
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

  override def grammars = ScalaConfig.grammars.get
  override def effacers = ScalaConfig.effacers
  override def syntaxers = ScalaConfig.syntaxers

  override protected def createIndenter = new ScalaIndenter(buffer, config)

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

  //
  // FNs

  override def electricNewline () {
    // shenanigans to determine whether we should auto-insert the doc prefix (* )
    if (commenter.inDoc(buffer, view.point())) {
      newline()
      view.point() = commenter.insertDocPre(buffer, view.point())
      reindentAtPoint()
    } else super.electricNewline()
  }

  // TODO: more things!
}
