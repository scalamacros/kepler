package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.tools.nsc.ast.parser.Tokens._
import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.compat.Platform.EOL

abstract class QParser extends ScalaParser {
  import global._
  val placeholders: Set[String]

  def wrapCode(code: String): String =
    "object wrapper {" + EOL + code + EOL + "}"

  def unwrapTree(wrappedTree: Tree): Tree = {
    val PackageDef(_, List(ModuleDef(_, _, Template(_, _, _ :: parsed)))) = wrappedTree
    parsed match {
      case tree :: Nil => tree
      case stats :+ tree => Block(stats, tree)
    }
  }

  def parse(code: String): Tree = {
    val file = new BatchSourceFile("<quasiquotes>", wrapCode(code))
    val tree = new QuasiQuoteParser(file).parse()
    unwrapTree(tree)
  }

  class QuasiQuoteParser(source0: SourceFile) extends SourceFileParser(source0) {
    // q"def foo($x)"
    override def allowTypelessParams = true

    // q"{ $x }"
    override def block(): Tree = makeBlock(blockStatSeq())
    private def makeBlock(stats: List[Tree]): Tree =
      if (stats.isEmpty) Literal(Constant())
      else if (!stats.last.isTerm) Block(stats, Literal(Constant()))
      else if (stats.length == 1) stats match {
        case Ident(TermName(name)) :: Nil if placeholders(name) => Block(stats.init, stats.last)
        case _ => stats.head
      } else Block(stats.init, stats.last)

    // q"foo match { $x }"
    override def caseClauses(): List[CaseDef] = {
      val cases = caseSeparated { atPos(in.offset)(treeBuilder.makeCaseDef(pattern(), guard(), caseBlock())) }
      if (cases.isEmpty) {
        if (in.token == IDENTIFIER && placeholders(in.name.toString)) ???
        else accept(CASE) // trigger error if there are no cases and noone gets spliced
      }
      cases
    }
  }
}


