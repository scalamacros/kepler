package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import java.util.UUID.randomUUID
import scala.collection.immutable.ListMap

trait Macros { self: Quasiquotes =>
  import c.universe._

  /** This is a shorcut variable that links to "$u" universe variable name.
   *  With the help of it it's possible to use $u inside of the quasiquote
   *  and it will have the same meaning in expanded code.
   */
  val u = nme.UNIVERSE_SHORT

  /** This trait abstracts over all variations of quasiquotes
   *  and allows to share core logic. The main differences are
   *  in parser, reifier and wrapping behaviour.
   */
  trait AbstractMacro {

    val parser: Parser

    /** Reifier factory that abstracts over different reifiers need for apply and unapply macros. */
    def reifier(universe: Tree, placeholders: Placeholders): Reifier

    /** Wraps reified tree into a final result of macro expansion. */
    def wrap(universe: Tree, reified: Tree): Tree

    /** Extracts universe tree, args trees and params strings from macroApplication. */
    def extract = c.macroApplication match {
      case q"$universe.Quasiquote($stringContext.apply(..$parts0)).${_}.${_}(..$args)" =>
        val parts = parts0.map {
          case Literal(Constant(s: String)) => s
          case part => c.abort(part.pos, "Quasiquotes can only be used with constant string arguments.")
        }
        if (args.length != parts.length - 1)
          c.abort(c.enclosingPosition, "Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ =>
        c.abort(c.macroApplication.pos, s"Couldn't parse call prefix tree ${c.macroApplication}.")
    }

    /** Generates scala code to be parsed by parser and placeholders map from incoming args and parts. */
    def generate(args: List[Tree], parts: List[String]): (String, Placeholders) = {
      val sb = new StringBuilder()
      var placeholders = ListMap[String, (Tree, Int)]()

      foreach2(args, parts.init) { (tree, p) =>
        val (part, cardinality) =
          if (p.endsWith("..."))
            (p.stripSuffix("..."), 2)
          else if (p.endsWith(".."))
            (p.stripSuffix(".."), 1)
          else
            (p, 0)
        val freshname = c.fresh(nme.QUASIQUOTE_PREFIX)
        sb.append(part)
        sb.append(freshname)
        placeholders += freshname -> (tree, cardinality)
      }
      sb.append(parts.last)

      (sb.toString, placeholders)
    }

    def debug(msg: String) =
      if (settings.Yquasiquotedebug.value) println(msg)

    /** Quasiquote macro expansion core logic. */
    def apply() = {
      val (universe, args, parts) = extract
      val (code, placeholders) = generate(args, parts)
      debug(s"\ncode to parse=\n$code\n")
      val tree = parser.parse(code, placeholders.keys.toSet)
      debug(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")
      val reified = reifier(universe, placeholders).reify(tree)
      debug(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")
      val result = wrap(universe, reified)
      debug(s"result tree\n=${result}\n=${showRaw(result)}\n")
      result
    }
  }

  trait ApplyMacro extends AbstractMacro {
    def reifier(universe: Tree, placeholders: Placeholders): Reifier =
      new ApplyReifier(universe, placeholders)
    def wrap(universe: Tree, reified: Tree): Tree =
      q"""{
        val $u: $universe.type = $universe
        $reified
      }"""
  }

  trait UnapplyMacro extends AbstractMacro {
    def reifier(universe: Tree, placeholders: Placeholders): Reifier =
      new UnapplyReifier(universe, placeholders)
    def wrap(universe: Tree, reified: Tree) = reified
  }

  trait TermParsing { val parser = TermParser }
  trait TypeParsing { val parser = TypeParser }

  def applyQ = (new ApplyMacro with TermParsing).apply()
  def applyTq = (new ApplyMacro with TypeParsing).apply()
  def unapplyQ = (new UnapplyMacro with TermParsing).apply()
  def unapplyTq = (new UnapplyMacro with TypeParsing).apply()
}