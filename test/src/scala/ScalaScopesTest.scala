//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.code

import org.junit.Assert._
import org.junit._
import scaled._
import scaled.grammar._
import scaled.impl.BufferImpl

class ScalaScopesTest {

  // @Test def dumpGrammar () {
  //   val plugin = new ScalaGrammarPlugin()
  //   plugin.grammar.print(System.out)
  // }

  val testScalaCode = Seq(
    //                1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/**",
    /* 3*/ " * This is some test Scala code that we'll use to test {@link Grammar} and",
    /* 4*/ " * the {@literal ScalaDoc} grammar.",
    /* 5*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 6*/ " */",
    /* 7*/ "class Test extends Baffle {",
    /* 8*/ "  /** A Scaladoc style comment!",
    /* 9*/ "    * @param foo for fooing. */",
    /*10*/ "  def foo (foo :Int) {}",
    /*11*/ "",
    /*12*/ "  /**",
    /*13*/ "   * A method. How exciting. Let's {@link Test} to something.",
    /*14*/ "   * @throws IllegalArgumentException if we feel like it.",
    /*15*/ "   */",
    /*16*/ "  @Deprecated(\"Use peanuts\")",
    /*17*/ "  def test (count :Int) :Int = count + 5",
    /*19*/ "}").mkString("\n")

  @Test def testStylesLink () {
    val buffer = BufferImpl(new TextStore("Test.scala", "", testScalaCode))
    val plugin = new ScalaGrammarPlugin()
    val scoper = Grammar.testScoper(Seq(plugin.grammar("source.scala")), buffer,
                                    List(new Selector.Processor(plugin.effacers)))
    scoper.rethinkBuffer()

    // println(scoper.showMatchers(Set("#code", "#class")))
    // val start = 0  ; val end = buffer.lines.length
    // start until end foreach { ll =>
    //   println(buffer.line(ll))
    //   scoper.showScopes(ll) foreach { s => println(ll + ": " + s) }
    // }

    assertFalse("Whitespace before doc comment not scoped as comment",
               scoper.scopesAt(Loc(8, 0)).contains("comment.block.documentation.scala"))
    assertTrue("Open doc comment is scoped as comment",
               scoper.scopesAt(Loc(8, 2)).contains("comment.block.documentation.scala"))
    assertTrue("Doc comment content is scoped as comment",
               scoper.scopesAt(Loc(8, 6)).contains("comment.block.documentation.scala"))
  }
}
