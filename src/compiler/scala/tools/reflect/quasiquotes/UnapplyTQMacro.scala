package scala.tools.reflect
package quasiquotes

abstract class UnapplyTQMacro extends UnapplyQMacro { self =>
  import ctx.universe._

  override def parse(code: String) = {
    val parser = new {
      val global: ctx.universe.type = ctx.universe
      val placeholders = self.placeholders.keys.toSet
    } with QParser
    parser.parse(code)
  }
}
