//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.grammar.{Scoper, Selector, Span}
import scaled.major.{CodeConfig, CodeMode}

object ScalaConfig extends ConfigDefs {
  import EditorConfig._
  import CodeConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val colorizers = Map(
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

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that applies
    * `cssClass` to buffer spans matched by the selector. */
  def effacer (selector :String, cssClass :String) =
    // TODO: first remove all code faces, then add the desired faces?
    (Selector.parse(selector), (buf :Buffer, span :Span) => {
      // println(s"Applying $cssClass to $span")
      buf.addStyle(cssClass, span)
    })
}

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends CodeMode(editor, config, view, disp) {

  // TEMP: for now use a TextMate grammar for code highlighting
  val scoper = new Scoper(Grammars.grammars, view.buffer)
  scoper.apply(new Selector.Processor(ScalaConfig.colorizers))

  override def configDefs = ScalaConfig :: super.configDefs
  override def keymap = super.keymap ++ Seq(
    "M-A-p" -> "show-syntax" // TODO: also M-PI?
  )
  override def dispose () {} // TODO: remove all colorizations?

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showSyntax () {
    val ss = scoper.scopesAt(view.point())
    view.popup() = Popup(if (ss.isEmpty) List("No scopes.") else ss, Popup.UpRight(view.point()))
  }

  // TODO: more things!
}
