package scala.tools.reflect
package quasiquotes

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.SortedMap

abstract class UnapplyQMacro extends Types { self =>
  val ctx: Context
  val global: ctx.universe.type = ctx.universe
  import ctx.universe._
  import ctx.universe.Flag._

  val (universe, parts) =
    ctx.macroApplication match {
      case Apply(Select(Select(Apply(Select(universe, _), List(Apply(_, parts0))), _), _), _) =>
        val parts = parts0.map{
          case Literal(Constant(s: String)) => s
          case _ => throw new Exception("") // empty exception?
        }
        (universe, parts)
      case _ => throw new Exception("") // empty exception?
    }

  if (!(parts.length >= 1 && parts.length <= 23))
    ctx.abort(ctx.enclosingPosition, "Inappropriate amount of quasiquote params.")

  val unapplySelector = Ident(nme.SELECTOR_DUMMY)
  unapplySelector.setType(treeType)

  val (code, placeholders) = {
    val sb = new StringBuilder()
    var placeholders = SortedMap[String, String]()

    parts.init.foreach { p =>
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
      placeholders += freshname -> cardinality
    }
    sb.append(parts.last)

    (sb.toString, placeholders)
  }

  if (settings.Yquasiquotedebug.value) println(s"code to parse=\n$code\n")

  val tree = parse(code)

  if (settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

  val (reifiedTree, correspondingTypes) = reifyTree(tree)

  if (settings.Yquasiquotedebug.value) println(s"corresponding types=\n$correspondingTypes\n")
  if (settings.Yquasiquotedebug.value) println(s"reified tree\n=${reifiedTree}\n=${showRaw(reifiedTree)}\n")

  val caseBody: Tree =
    if (placeholders.size == 0)
      Literal(Constant(true))
    else if (placeholders.size == 1)
      Apply(Ident(TermName("Some")), List(Ident(TermName(placeholders.keys.head))))
    else
      Apply(
        Ident(TermName("Some")),
        List(Apply(
          Ident(TermName("Tuple" + placeholders.size)),
          placeholders.map(p => Ident(TermName(p._1))).toList)))


  val unapplyBody =
    Block(
      List(Import(Ident(TermName("$u")), List(ImportSelector(nme.WILDCARD, 0, null, 0)))),
      Match(Ident(TermName("tree")), List(
        CaseDef(reifiedTree, EmptyTree, caseBody),
        CaseDef(Ident(nme.WILDCARD), EmptyTree, Ident(TermName("None"))))))

  val localTreeType = Select(Ident(TermName("$u")), TypeName("Tree"))

  val unapplyResultType: Tree =
    if (placeholders.size == 0){
      Ident(TypeName("Boolean"))
    } else if (placeholders.size == 1){
      AppliedTypeTree(Ident(TypeName("Option")), List(correspondingTypes(placeholders.keys.head)))
    } else {
      val tuple = Ident(TypeName("Tuple" + placeholders.size))
      val treetuple = AppliedTypeTree(tuple, placeholders.map(p => correspondingTypes(p._1)).toList)
      val optiontreetuple = AppliedTypeTree(Ident(TypeName("Option")), List(treetuple))
      optiontreetuple
    }

  val apiUniverseType = Select(Select(Select(Ident(TermName("scala")), TermName("reflect")), TermName("api")), TypeName("Universe"))

  val unapplyMethod =
    DefDef(
      Modifiers(), TermName("unapply"), List(),
      List(
        List(ValDef(Modifiers(PARAM), TermName("$u"), apiUniverseType, EmptyTree)),
        List(ValDef(Modifiers(PARAM), TermName("tree"), localTreeType, EmptyTree))),
      unapplyResultType,
      unapplyBody)

  val moduleName = TermName(nme.QUASIQUOTE_MATCHER_NAME + randomUUID().toString.replace("-", ""))

  val moduleDef =
    ModuleDef(Modifiers(), moduleName, Template(
      List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
      emptyValDef,
      List(
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
        unapplyMethod)))

  if (settings.Yquasiquotedebug.value) println(s"moduledef\n=${showRaw(moduleDef, printTypes=true, printIds=true)}\n=$moduleDef\n")

  ctx.introduceTopLevel(nme.QUASIQUOTE_MATCHER_PACKAGE.toString, moduleDef)

  val moduleRef = Select(Ident(nme.QUASIQUOTE_MATCHER_PACKAGE), moduleName)
  val result = Apply(Apply(Select(moduleRef, TermName("unapply")), List(universe)), List(unapplySelector))

  def parse(code: String) = {
    val parser = new {
      val global: ctx.universe.type = ctx.universe
      val placeholders = self.placeholders.keys.toSet
    } with Parser
    parser.parse(code)
  }

  def reifyTree(tree: Tree) = {
    val ctx0 = ctx
    val placeholders0 = placeholders
    val universe0 = universe
    val reifier = new {
      val ctx: ctx0.type = ctx0
      val global: ctx0.universe.type = ctx0.universe
      val universe = universe0.asInstanceOf[global.Tree]
      val placeholders = placeholders0
      val mirror = EmptyTree.asInstanceOf[global.Tree]
      val typer = null
      val reifee = null
      val concrete = false
    } with UnapplyReifier
    val reifiedtree = reifier.reify(tree.asInstanceOf[reifier.global.Tree]).asInstanceOf[Tree]
    val correspondingTypes = reifier.correspondingTypes.map { pair =>
      val tpe = pair._2.asInstanceOf[global.Type]
      if (tpe =:= termNameType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("TermName")))
      else if (tpe =:= typeNameType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("TypeName")))
      else if (tpe =:= nameType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("Name")))
      else if (tpe =:= treeType)
        (pair._1, Select(Ident(TermName("$u")), TypeName("Tree")))
      else if (tpe =:= listTreeType)
        (pair._1,
          AppliedTypeTree(
            Ident(TypeName("List")),
            List(Select(Ident(TermName("$u")), TypeName("Tree")))))
      else if (tpe =:= listListTreeType)
        (pair._1,
          AppliedTypeTree(
            Ident(TypeName("List")),
            List(AppliedTypeTree(
                Ident(TypeName("List")),
                List(Select(Ident(TermName("$u")), TypeName("Tree")))))))
      else
        ctx.abort(ctx.enclosingPosition, s"Unexpected reified type $tpe.")
    }
    (reifiedtree, correspondingTypes)
  }
}
