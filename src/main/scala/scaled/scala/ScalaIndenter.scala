//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.util.{Chars, Block, Indenter}

object ScalaIndenter {
  import Indenter._
  import Chars._

  /** If the previous line ends with `=`, insets this line by one step relative to it. */
  class ValueExprBody (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      // seek backward to the first non-whitespace character
      val pc = buffer.scanBackward(isNotWhitespace, pos, block.start)
      // if it's not on the preceding line, or it's not an '=', we're inapplicable
      if (pc.row != pos.row-1 || buffer.charAt(pc) != '=') None
      else {
        debug(s"Indenting one step from 'foo =' @ $pc")
        Some(indentFrom(readIndent(buffer, pc), 1))
      }
    }
  }

  /** If we're in a `case` statement's pseudo-block, inset this line one step from the case. */
  class CaseBody (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    private val caseM = Matcher.exact("case")
    private val arrowM = Matcher.exact("=>")
    private val closeB = Matcher.exact("}")

    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      // if we're looking at 'case' or '}' then don't apply this rule
      if (startsWith(line, caseM) || startsWith(line, closeB)) None
      // otherwise if the first line after the start of our block is 'case ... =>' then we're in a
      // case pseudo block, so indent relative to the 'case' not the block
      else {
        val caseLine = buffer.line(block.start.nextL)
        if (!startsWith(caseLine, caseM) || !endsWith(caseLine, arrowM)) None
        else {
          debug(s"Identing one step from 'case' @ ${block.start.nextL}")
          Some(indentFrom(readIndent(caseLine), 1))
        }
      }
    }
  }
}
