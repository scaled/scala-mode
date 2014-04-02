//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/samskivert/scaled-scala-mode/blob/master/LICENSE

package scaled.code

import scaled._

@Major(name="scala",
       tags=Array("code", "project", "scala"),
       pats=Array(".*\\.scala"),
       ints=Array("scala"),
       desc="A major editing mode for the Scala language.")
class ScalaMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends CodeMode(editor, config, view, disp) {

  override def dispose () {} // TODO

  // TODO: things!
}
