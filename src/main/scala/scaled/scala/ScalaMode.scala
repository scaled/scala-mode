//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.grammar._
import scaled.major.CodeConfig

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

  override def configDefs = ScalaConfig :: super.configDefs
  override protected def grammars = ScalaConfig.grammars
  override protected def effacers = ScalaConfig.effacers

  // TODO: more things!
}
