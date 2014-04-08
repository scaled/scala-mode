//
// Scaled Scala Mode - a Scaled major mode for editing Scala code
// http://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import scaled.grammar._

object Grammars {

  // def htmlGrammar = Grammar.parse(stream("HTML.tmLanguage"))
  def scalaGrammar = Grammar.parse(stream("Scala.tmLanguage"))
  lazy val grammars = Seq(scalaGrammar)

  private def stream (path :String) = getClass.getClassLoader.getResourceAsStream(path)
}
