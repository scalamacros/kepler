package scala.tools.reflect
package quasiquotes

import scala.reflect.macros.runtime.Context

abstract class Quasiquotes extends Macros
                              with Parsers
                              with Reifiers
                              with Compat {
  val c: Context
  val global: c.universe.type = c.universe
  import c.universe._

  def debug(msg: String): Unit =
    if (settings.Yquasiquotedebug.value) println(msg)

  def require(cond: Boolean, pos: => Position, msg: String): Unit =
    if (!cond) c.abort(pos, msg)
}
