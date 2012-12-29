package scala.tools.reflect
package quasiquotes

abstract class TQParser extends QParser {
  import global._

  override def wrapCode(code: String) = super.wrapCode("type T = " + code)

  override def unwrapTree(wrappedTree: Tree): Tree = {
    val TypeDef(_, _, _, rhs) = super.unwrapTree(wrappedTree)
    rhs
  }
}