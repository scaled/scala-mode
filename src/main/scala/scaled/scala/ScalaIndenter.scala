//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.scala

import scaled._
import scaled.code.Indenter

class ScalaIndenter (buf :Buffer, cfg :Config) extends Indenter.ByBlock(buf, cfg) {
  import Indenter._

  override def computeIndent (state :State, base :Int, line :LineV, first :Int) = {
    // bump extends/with in two indentation levels
    if (line.matches(extendsOrWithM, first)) base + 2*indentWidth
    // if we're in a faux case block...
    else if (state.isInstanceOf[CaseS]) {
      // ignore the block indent for subsequent case statements
      if (line.matches(caseArrowM, first)) base - indentWidth
      // ignore the block indent for the final close bracket
      else if (line.charAt(first) == '}') base - 2*indentWidth
      // otherwise stick to business as usual...
      else super.computeIndent(state, base, line, first)
    }
    else super.computeIndent(state, base, line, first)
  }

  override protected def createStater = new BlockStater() {
    private[this] var opensSLB = false
    private[this] var slbExprOpen = -1
    private[this] var slbExprClose = -1
    private[this] var slbExpectsPair = false

    override def adjustStart (line :LineV, first :Int, last :Int, start :State) :State = {
      // reset our SLB tracking state
      slbExprOpen = -1 ; slbExprClose = -1 ; slbExpectsPair = false
      // if we're looking at an SLB, push a state for it
      opensSLB = line.matches(singleLineBlockM, first)
      if (opensSLB) {
        val nstate = new SingleBlockS(singleLineBlockM.group(1), first, start)
        // if this SLB has no associated expression (else or a do); set the expression open/close
        // column to the start of the block so that the "pop on later block" code works properly
        if (nstate.lacksExpr) {
          slbExprOpen = first ; slbExprClose = first
        }
        // if this is an 'if' or 'else if', or a 'do', we want to know whether or not to expect to
        // see a subsequent 'else' or 'while' so that we can determine if this statement should
        // terminate a continued statement chain; we check to see whether that expected pair
        // already occurs on this same line, in which case we don't expect it later; note that
        // it's possible for an 'if' or 'else if' to simply not be followed by an 'else', and in
        // that case we can potentially do the wrong thing, but there's only so much we can do
        // without a full fledged Scala parser
        slbExpectsPair = nstate.expectsPair(line)
        nstate
      }
      // if this line opens a block or doc comment, push a state for it
      else if (countComments(line, first) > 0) {
        // if this is a doc comment which is followed by non-whitespace, then indent to match the
        // second star rather than the first
        val col = if (line.matches(firstLineDocM, first)) 2 else 1
        new CommentS(col, start)
      }
      // if this line opens a match case which does not contain any code after the arrow, create a
      // faux block to indent the case body
      else if (line.matches(caseArrowM, first) && line.charAt(last) == '>') {
        // if we're currently in the case block for the preceding case, pop it first
        new CaseS(start.popIf(_.isInstanceOf[CaseS]))
      }
      // otherwise leave the start as is
      else start
    }

    override def adjustEnd (line :LineV, first :Int, last :Int, start :State, cur :State) :State = {
      var end = cur
      // if this line closes a doc/block comment, pop our comment state from the stack
      if (countComments(line, 0) < 0) end = end.popIf(_.isInstanceOf[CommentS])

      // if the last non-ws-non-comment char is beyond our SLB condition expression then pop the
      // SLB state because the "body" was on the same line (this is normally done when we see any
      // sort of bracket after our SLB expr, but it's possible that the SLB body contains no
      // brackets, so we catch that case here)
      if (opensSLB && last > slbExprClose) {
        end = end.popIf(_.isInstanceOf[SingleBlockS])
        opensSLB = false
      }

      // if this line is blank or contains only comments; do not mess with our "is continued or
      // not" state; wait until we get to a line with something actually on it
      if (line.synIndexOf(s => !s.isComment, first) == -1) end
      else {
        // determine (heuristically) whether this line appears to be a complete statement
        val isContinued = (last >= 0) && contChars.indexOf(line.charAt(last)) >= 0
        val isComplete = !(isContinued || slbExpectsPair ||
                           end.isInstanceOf[BlockS] || end.isInstanceOf[ExprS])

        // if we appear to be a complete statement, pop any continued statement state off the stack
        if (isComplete) end.popIf(_.isInstanceOf[ContinuedS])
        // if we're not already a continued statement, we may need to start being so
        else if (isContinued) new ContinuedS(end.popIf(_.isInstanceOf[ContinuedS]))
        // otherwise stick with what we have
        else end
      }
    }

    override def openBlock (line :LineV, open :Char, close :Char, col :Int, state :State) :State = {
      var top = state
      if (opensSLB) {
        // if we're processing an SLB and this is the first block on the line, note its info
        if (slbExprOpen == -1) slbExprOpen = col
        // if we're opening another block after our SLB token's expression block, then pop the SLB
        // state because we're either opening a multi-line block or we're seeing an expression
        // which is cuddled onto the same line as the SLB; in either case we don't want our SLB
        // state to cause the next line to be indented
        else if (slbExprClose != -1) {
          top = top.popIf(_.isInstanceOf[SingleBlockS])
          opensSLB = false
        }
      }
      super.openBlock(line, open, close, col, top)
    }
    override def closeBlock (line :LineV, close :Char, col :Int, state :State) :State = {
      // if we're closing the bracketed expr that goes along with our SLB, note the close column
      if (opensSLB) state match {
        case es :ExprS if (es.col == slbExprOpen) => slbExprClose = col
        case _ => // ignore
      }
      super.closeBlock(line, close, col, state)
    }

    protected def contChars = ".+-="
  }

  protected class CommentS (inset :Int, next :State) extends State(next) {
    override def indent (config :Config, top :Boolean) = inset + next.indent(config)
    override def show = s"CommentS($inset)"
  }
  protected class ContinuedS (next :State) extends State(next) {
    override def show = "ContinuedS"
  }
  protected class CaseS (next :State) extends State(next) {
    override def show = "CaseS"
  }
  protected class SingleBlockS (token :String, col :Int, next :State) extends State(next) {
    def expectsPair (line :LineV) = token match {
      // if our if or else if is followed by an else on the same line, we're already paired
      case "if" | "else if" => line.lastIndexOf(elseM) match {
        case -1 => true // no else, we expect one
        case ii => ii == line.lastIndexOf(elseIfM) // the else we saw was actually an else if
      }
      case "do" => line.indexOf(whileM) == -1
      case _ => false
    }
    def lacksExpr = token == "else" || token == "do"
    // if the single-block state is on the top of the stack, then we're in the line immediately
    // following the single-block statement, so we want to indent
    override def indent (config :Config, top :Boolean) =
      (if (top) indentWidth(config) else 0) + next.indent(config)
    override def show = s"SingleBlockS($token, $col)"
  }

  private val caseArrowM = Matcher.regexp("""case\s.*=>""")
  private val extendsOrWithM = Matcher.regexp("""(extends|with)\b""")
  private val singleLineBlockM = Matcher.regexp("""(if|else if|else|while)\b""")

  private val firstLineDocM = Matcher.regexp("/\\*\\*\\s*\\S+")

  private val elseIfM = Matcher.regexp("""\belse\s+if\b""")
  private val elseM = Matcher.regexp("""\belse\b""")
  private val whileM = Matcher.regexp("""\bwhile\b""")
}
