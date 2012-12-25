package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable

abstract class ApplyQMacro { self =>
  val ctx: Context
  import ctx.universe._

  def codePrefix = ""

  val (universe, args, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), args) =>
        val parts = parts0.map{
          case Literal(Constant(s: String)) => s
          case part => ctx.abort(part.pos, "Quasi-quotes can only be used with constant string arguments.")
        }
        if (args.length != parts.length - 1)
          ctx.abort(ctx.enclosingPosition, "Imbalanced amount of arguments.")
        (universe, args, parts)
      case _ => ctx.abort(ctx.macroApplication.pos, "Couldn't parse call prefix tree ${ctx.macroApplication}.")
    }

  val (code, subsmap) = {
    val sb = new StringBuilder(codePrefix)
    val subsmap = mutable.Map[String, (Tree, String)]()

    foreach2(args, parts.init) { (tree, p) =>
      val (part, cardinality) =
        if (p.endsWith("..."))
          (p.substring(0, p.length - 3), "...")
        else if (p.endsWith(".."))
          (p.substring(0, p.length - 2), "..")
        else
          (p, "")
      val freshname = ctx.fresh(nme.QUASIQUOTE_PREFIX)
      sb.append(part)
      sb.append(freshname)
      subsmap += freshname -> (tree, cardinality)
    }
    sb.append(parts.last)

    (sb.toString, subsmap.toMap)
  }

  if (settings.Yquasiquotedebug.value) println(s"\ncode to parse=\n$code\n")

  val tree = parse(code)

  if (settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val reified = reifyTree(beforeReify(tree))

  if (settings.Yquasiquotedebug.value) println(s"reified tree\n=${reified}\n=${showRaw(reified)}\n")

  val result = wrap(reified)

  if (settings.Yquasiquotedebug.value) println(s"result tree\n=${result}\n=${showRaw(result)}\n")

  def parse(code: String) = {
    val parser = new {
      val global: ctx.universe.type = ctx.universe
      val placeholders = self.subsmap.keys.toSet
    } with Parser
    parser.parse(code)
  }

  def beforeReify(tree: Tree) = tree

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val subsmap0 = subsmap
    val universe0 = universe
    val reifier = new {
      val ctx: ctx0.type = ctx0
      val global: ctx0.universe.type = ctx0.universe
      val subsmap: Map[String, (global.Tree, String)] = subsmap0.map {
        case (name, (tree, cardinality)) => (name, (tree.asInstanceOf[global.Tree], cardinality))
      }
      val universe = universe0.asInstanceOf[global.Tree]
      val mirror = global.EmptyTree
      val typer = null
      val reifee = null
      val concrete = false
    } with ApplyReifier
    reifier.reify(tree.asInstanceOf[reifier.global.Tree]).asInstanceOf[Tree]
  }

  def wrap(t: Tree) =
    Block(
      List(ValDef(Modifiers(),
        nme.UNIVERSE_SHORT,
        SingletonTypeTree(universe),
        universe)),
      t)
}