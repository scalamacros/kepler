package scala.tools.reflect
package quasiquotes

abstract class ApplyTQMacro extends ApplyQMacro {
  import ctx.universe._

  override def codePrefix = "type T ="

  override def beforeReify(tree: Tree): Tree = {
    val TypeDef(_, _, _, rhs) = tree
    rhs
  }
}
