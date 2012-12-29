package scala.tools.reflect
package quasiquotes

abstract class ApplyTQMacro extends ApplyQMacro { self =>
  import ctx.universe._

  override def parse(code: String) = {
    val parser = new {
      val global: ctx.universe.type = ctx.universe
      val placeholders = self.subsmap.keys.toSet
    } with TQParser
    parser.parse(code)
  }
}
