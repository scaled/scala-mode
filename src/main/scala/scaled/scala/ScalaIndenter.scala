//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.code.{CodeConfig, Block, Indenter}
import scaled.util.Chars

object ScalaIndenter {
  import Indenter._
  import Chars._

  // matchers used by various bits below
  val ctoM = Matcher.regexp("""\b(class|trait|object)\b""")
  val extendsOrWithM = Matcher.regexp("""(extends|with)\b""")

  /** Handles reading block (and pseudo-block) indent for Scala code. This checks for wrapped
    * `extends` and `with` clauses before falling back to the standard [[readIndentSkipArglist]].
    */
  def readBlockIndent (ctx :Context, pos :Loc) :Int =
    readBlockIndent(ctx, ctx.blocker.require(pos, Syntax.Default), pos)

  /** Handles reading block (and pseudo-block) indent for Scala code. This checks for wrapped
    * `extends` and `with` clauses before falling back to the standard [[readIndentSkipArglist]].
    * @param block the block that encloses `pos`. Use the two arg version of this method if that
    * block is not handy, and it will be computed.
    */
  def readBlockIndent (ctx :Context, block :Block, pos :Loc) :Int = {
    // if we're looking at extends or with, move back to the line that contains "class", "trait" or
    // "object" and indent relative to that
    if (startsWith(ctx.buffer.line(pos), extendsOrWithM)) {
      findCodeBackward(ctx, ctoM, pos.atCol(0), block) match {
        case Loc.None => println(s"Missing $ctoM for block ($block) on $extendsOrWithM line!") ; 0
        case      loc => readIndent(ctx.buffer, loc)
      }
    }
    // otherwise fall back to readIndentSkipArglist
    else readIndentSkipArglist(ctx.buffer, pos)
  }

  /** If the previous line ends with `=` (indicating a continued value expression) or a `.`
    * (indicating a continued method select chain), insets this line by one step relative to it. */
  class ContinuedExpr (ctx :Context) extends PrevLineEnd(ctx) {
    def apply (block :Block, line :LineV, pos :Loc, prevPos :Loc) :Option[Int] = {
      // if the line doesn't end with '=', we're inapplicable
      val c = buffer.charAt(prevPos)
      if (c != '=' && c != '.') None
      else {
        debug(s"Indenting one step from 'foo $c' @ $prevPos")
        Some(indentFrom(readBlockIndent(ctx, prevPos), 1))
      }
    }
  }

  /** If we're in a `case` statement's pseudo-block, inset this line one step from the case. */
  class CaseBody (ctx :Context) extends Indenter(ctx) {
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

  /** Indents `extends` relative to a preceding `(class|trait|object)` line. */
  class Extends (ctx :Context) extends Indenter(ctx) {
    private val extendsM = Matcher.regexp("""extends\b""")

    def apply (block :Block, line :LineV, pos :Loc) :Option[Int] = {
      if (!line.matches(extendsM, pos.col)) None
      else findCodeBackward(ctoM, pos, block) match {
        case Loc.None => None
        case loc =>
          debug(s"Indenting extends relative to class/object @ $loc")
          Some(indentFrom(readIndent(buffer, loc), 2))
      }
    }
  }
}
