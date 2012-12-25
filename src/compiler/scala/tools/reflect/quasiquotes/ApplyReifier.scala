package scala.tools.reflect
package quasiquotes

import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros

abstract class ApplyReifier extends ReflectReifier with Types {
  import global._

  val subsmap: Map[String, (Tree, String)]
  val ctx: macros.Context

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
      if (subsmap.contains(name))
        Some(name)
      else
        None
    }
  }

  object SubsToTree {

    def unapply(name: Name): Option[(Tree, String)] =
      unapply(name.toString)

    def unapply(name: String): Option[(Tree, String)] =
      subsmap.get(name).flatMap { case (tree, card) =>
        if (tree.tpe <:< treeType) {
          if (card != "")
            throw new Exception(s"Incorrect cardinality, expected '', got '$card'")
          Some((tree, card))
        } else if (tree.tpe <:< listTreeType) {
          if (card != "..")
            throw new Exception(s"Incorrect cardinality, expected '..', but got '$card'")
          Some((tree, card))
        } else if (tree.tpe <:< listListTreeType) {
          if (card != "...")
            throw new Exception(s"Incorrect cardinality, expected '...', but got '$card'")
          Some((tree, card))
        } else {
          val liftType = appliedType(liftableType, List(tree.tpe))
          val lift = ctx.inferImplicitValue(liftType.asInstanceOf[ctx.Type], silent = true).asInstanceOf[Tree]
          if (lift != EmptyTree) {
            if (card != "")
              throw new Exception(s"Incorrect cardinality, expected '', but got '$card'")
            Some((wrapLift(lift, tree), card))
          } else
            None
        }
      }

    def wrapLift(lift: Tree, tree: Tree) =
      TypeApply(
        Select(Apply(lift, List(universe, tree)), newTermName("asInstanceOf")),
        List(Select(Ident(newTermName("$u")), newTypeName("Tree"))))
  }

  object SubsToNameTree {

    def unapply(name: Name): Option[Tree] =
      subsmap.get(name.toString).collect { case (tree, _) if tree.tpe <:< nameType => tree }
  }

  override def reifyTree(tree: Tree) = reifyBasicTree(tree)

  override def reifyBasicTree(tree: Tree): Tree = {
    tree match {
      case SimpleTree(SubsToTree(tree, card)) => tree
      case _ => super.reifyBasicTree(tree)
    }
  }

  override def reifyName(name: Name): Tree =
    if (!subsmap.contains(name.toString))
      super.reifyName(name)
    else
      name match {
        case SubsToNameTree(tree) => tree
        case _ => throw new Exception(s"Name expected but ${subsmap(name.toString)._1.tpe} found [$name:${subsmap(name.toString)}]")
    }

  override def reifyList(xs: List[Any]): Tree =
    Select(
      mkList(xs.map {
        case SimpleTree(SubsToTree(tree, card)) if card == ".." => tree
        case List(SimpleTree(SubsToTree(tree, card))) if card == "..." => tree
        case x @ _ => mkList(List(reify(x)))
      }),
      TermName("flatten"))
}