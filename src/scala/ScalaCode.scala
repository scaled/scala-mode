//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import scaled._
import scaled.util.Errors

/**
 * Utility routines for manipulating Scala code.
 */
object ScalaCode {

  private val importM = Matcher.regexp("^import ")
  private val packageM = Matcher.regexp("^package ")
  private val firstDefM = Matcher.regexp("(class|interface|object|trait)")

  /**
   * Inserts an import statement into `buffer` for `fqName`. TODO:
   * - handle groups of imports separated by spaces
   * - handle inserting import into condensed imports (i.e. import foo.{Bar, Baz})
   */
  def insertImport (buffer :Buffer, fqName :String) :Unit = {
    val text = s"import $fqName"

    // first figure out where we're going to stop looking
    val firstDef = buffer.findForward(firstDefM, buffer.start) match {
      case Loc.None => buffer.end
      case loc => loc
    }

    // TODO: handle fancy scala grouped imports...

    // look for an existing "import " statement in the buffer and scan down from there to find the
    // position at which to insert the new statement
    def loop (prev :Loc) :Loc = {
      val next = buffer.findForward(importM, prev.nextStart, firstDef)
      // if we see no more import statements...
      if (next == Loc.None) {
        // if we saw at least one import statement, then insert after the last one we saw
        if (prev != buffer.start) prev.nextStart
        // otherwise fail the search and fall back to inserting after 'package'
        else Loc.None
      }
      else {
        val ltext = buffer.line(next).asString
        // if we have this exact import, abort (we'll catch and report this below)
        if (ltext == text) throw Errors.feedback(s"$fqName already imported.")
        // if our import sorts earlier than this import, insert here
          else if (text < ltext) next
        // otherwise check the next import statement
          else loop(next)
      }
    }

    val (loc, lines) = loop(buffer.start) match {
      // if we failed to find existing import statements, look for a package statement
      case Loc.None => buffer.findForward(packageM, buffer.start, firstDef) match {
        case Loc.None =>
          // fuck's sake, put the import at the top of the file (with a blank line after)
          (buffer.start, List(text, "", ""))
        case loc =>
          // insert a blank line after 'package' and then our import
          (loc.nextStart, List("", text, ""))
      }
      case loc =>
        // put the import at the specified location
        (loc, List(text, ""))
    }
    buffer.insert(loc, lines map (Line.apply))
  }

}
