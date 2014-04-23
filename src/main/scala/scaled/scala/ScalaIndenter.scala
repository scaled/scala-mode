//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.major.CodeConfig
import scaled.util.{Chars, Block, Indenter}

object ScalaIndenter {
  import Indenter._
  import Chars._

  /** Handles reading block (and pseudo-block) indent for Scala code. This checks for wrapped
    * `extends` and `with` clauses before falling back to the standard [[readIndentSkipArglist]].
    */
  def readBlockIndent (buffer :BufferV, pos :Loc) :Int = {
    // if we're looking at extends or with, move back to the line that contains "class" or "object"
    // and indent relative to that
    if (startsWith(buffer.line(pos), extendsOrWithM)) {
      buffer.findBackward(classOrObjectM, pos.atCol(0)) match {
        case Loc.None => println(s"Missing (object|class) for block on (with|extends) line!") ; 0
        case      loc => readIndent(buffer, loc)
      }
    }
    // otherwise fall back to readIndentSkipArglist
    else readIndentSkipArglist(buffer, pos)
  }
  private val classOrObjectM = Matcher.regexp("""\b(class|object)\b""")
  private val extendsOrWithM = Matcher.regexp("""(extends|with)\b""")

  /** If the previous line ends with `=`, insets this line by one step relative to it. */
  class ValueExprBody (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      // seek backward to the first non-whitespace character
      val pc = buffer.scanBackward(isNotWhitespace, pos, block.start)
      // if it's not on the preceding line, or it's not an '=', we're inapplicable
      if (pc.row != pos.row-1 || buffer.charAt(pc) != '=') None
      else {
        debug(s"Indenting one step from 'foo =' @ $pc")
        Some(indentFrom(readBlockIndent(buffer, pc), 1))
      }
    }
  }

  /** If we're in a `case` statement's pseudo-block, inset this line one step from the case. */
  class CaseBody (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    private val caseArrowM = Matcher.regexp("""case\s.*=>""")
    private val closeB = Matcher.exact("}")

    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] =
      // if we're looking at 'case...=>' or '}' then don't apply this rule
      if (startsWith(line, caseArrowM) || startsWith(line, closeB)) None
      // otherwise if the first line after the start of our block is 'case ... =>' then we're in a
      // case pseudo block, so indent relative to the 'case' not the block
      else {
        // TODO: either skip comments, or search for caseArrowM and then make sure it is in our
        // same block... meh
        val caseLine = buffer.line(block.start.nextL)
        if (!startsWith(caseLine, caseArrowM)) None
        else {
          debug(s"Identing one step from 'case' @ ${block.start.nextL}")
          Some(indentFrom(readIndent(caseLine), 1))
        }
      }
  }

  /** Indents `extends` relative to a preceding `(class|object)` line. */
  class Extends (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    private val classObjectM = Matcher.regexp("""\b(class|object)\b""")
    private val extendsM = Matcher.regexp("""extends\b""")

    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      if (!line.matches(extendsM, pos.col)) None
      else buffer.findBackward(classObjectM, pos, block.start) match {
        case Loc.None => None
        case loc =>
          debug(s"Indenting extends relative to class/object @ $loc")
          Some(indentFrom(readIndent(buffer, loc), 2))
      }
    }
  }

  /** Aligns subsequent and final lines in Scaladoc comments on the second `*`. */
  class Scaladoc (config :Config, buffer :BufferV) extends Indenter(config, buffer) {
    private val starM = Matcher.exact("*")

    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] =
      if (!buffer.stylesAt(pos).contains(CodeConfig.docStyle) || !startsWith(line, starM)) None
      else {
        // scan back to the first line of the comment and indent two from there; the logic is
        // slightly weirded to ensure that we don't go past the start of the buffer even if the
        // situation lacks sanity
        var row = math.max(pos.row-1, 0)
        while (row > 0 && startsWith(buffer.line(row), starM)) row -= 1
        debug(s"Aligning scaladoc * with comment start on row $row.")
        Some(readIndent(buffer.line(row)) + 2)
      }
  }
}
