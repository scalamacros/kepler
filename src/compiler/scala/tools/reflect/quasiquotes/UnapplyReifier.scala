package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.SortedMap
import scala.collection.mutable

abstract class UnapplyReifier extends ReflectReifier with Types {
  import global._
  import global.treeInfo._

  val placeholders: SortedMap[String, String]
  val correspondingTypes: mutable.Map[String, Type] = mutable.Map()

  // Extractor that matches simple identity-like trees which
  // correspond to placeholders within quasiquote.
  object SimpleTree {

    def unapply(tree: Tree): Option[String] = {
      val name = tree match {
        case Ident(name) => name.toString
        case TypeDef(_, name, List(), TypeBoundsTree(
          Select(Select(Ident(TermName("_root_")), TermName("scala")), TypeName("Nothing")),
          Select(Select(Ident(TermName("_root_")), TermName("scala")), TypeName("Any")))) => name.toString
        case ValDef(_, name, TypeTree(), EmptyTree) => name.toString
        case _ => ""
      }
      if (placeholders.contains(name))
        Some(name)
      else
        None
    }
  }

  override def reifyTree(tree: Tree) = reifyBasicTree(tree)

  override def reifyBasicTree(tree: Tree): Tree = tree match {
    case SimpleTree(name) =>
      correspondingTypes(name) = treeType
      Bind(TermName(name), Ident(nme.WILDCARD))
    case Applied(fun, targs, argss) if fun != tree =>
      if (targs.length > 0)
        mirrorBuildCall("Applied", reify(fun), reifyList(targs), reifyList(argss))
      else
        mirrorBuildCall("Applied2", reify(fun), reifyList(argss))
    case global.emptyValDef =>
      mirrorBuildCall("EmptyValDefLike")
    case global.pendingSuperCall =>
      mirrorBuildCall("PendingSuperCallLike")
    case _ =>
      super.reifyBasicTree(tree)
  }

  override def scalaFactoryCall(name: String, args: Tree*): Tree =
    call("scala." + name, args: _*)

  override def reifyName(name: Name): Tree = {
    if (!placeholders.contains(name.toString))
      super.reifyName(name)
    else {
      correspondingTypes(name.toString) = nameType
      Bind(TermName(name.toString), Ident(nme.WILDCARD))
    }
  }
  override def reifyModifiers(m: global.Modifiers) =
    mirrorFactoryCall(nme.Modifiers, mirrorBuildCall("FlagsAsBits", reify(m.flags)), reify(m.privateWithin), reify(m.annotations))

  override def reifyList(xs: List[Any]): Tree = {
    val last = if (xs.length > 0) xs.last else EmptyTree
    last match {
      case SimpleTree(name) if placeholders(name) == ".." =>
        correspondingTypes(name) = listTreeType
        val bnd = Bind(TermName(name), Ident(nme.WILDCARD))
        xs.init.foldRight[Tree](bnd) { (el, rest) =>
          scalaFactoryCall("collection.immutable.$colon$colon", reify(el), rest)
        }
      case List(SimpleTree(name)) if placeholders(name) == "..." =>
        correspondingTypes(name) = listListTreeType
        val bnd = Bind(TermName(name), Ident(nme.WILDCARD))
        xs.init.foldRight[Tree](bnd) { (el, rest) =>
          scalaFactoryCall("collection.immutable.$colon$colon", reify(el), rest)
        }
      case _ =>
        super.reifyList(xs)
    }
  }
}

