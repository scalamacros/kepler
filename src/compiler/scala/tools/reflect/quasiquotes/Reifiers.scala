package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global
import scala.reflect.reify.{Reifier => ReflectReifier}
import scala.reflect.macros
import scala.collection.{immutable, mutable}

trait Reifiers { self: Quasiquotes =>
  import global._
  import global.build.SyntacticalClassDef
  import global.Flag._
  import global.treeInfo._
  import global.definitions._

  case class Placeholders(underlying: immutable.ListMap[String, (Tree, Int)]) {
    val accessed = mutable.Set[String]()
    def keys = underlying.keys
    def contains(key: String) = underlying.contains(key)
    def apply(key: String) = {
      accessed += key
      underlying(key)
    }
    def get(key: String) = {
      accessed += key
      underlying.get(key)
    }
  }

  abstract class Reifier(val universe: Tree, val placeholders: Placeholders) extends {
    val global: self.global.type = self.global
    val mirror = EmptyTree
    val typer = null
    val reifee = null
    val concrete = false

  } with ReflectReifier with Types {
    // shortcut
    val u = universe

    /** Extractor that matches simple identity-like trees which
     *  correspond to placeholders within quasiquote.
     */
    object Placeholder {
      def unapply(tree: Tree): Option[String] = tree match {
        case Ident(PlaceholderName(name)) => Some(name)
        case TypeDef(_, PlaceholderName(name), List(), TypeBoundsTree(
          Select(Select(Ident(nme.ROOTPKG), nme.scala_), tpnme.Nothing),
          Select(Select(Ident(nme.ROOTPKG), nme.scala_), tpnme.Any))) => Some(name)
        case ValDef(_, PlaceholderName(name), TypeTree(), EmptyTree) => Some(name)
        case _ => None
      }
    }

    object PlaceholderName {
      def unapply(name: Name): Option[String] =
        if (placeholders.contains(name.toString))
          Some(name.toString)
        else
          None
    }

    object AnnotPlaceholder {
      def unapply(tree: Tree): Option[(String, List[Tree])] = tree match {
        case Apply(Select(New(Placeholder(name)), nme.CONSTRUCTOR), args) => Some((name, args))
        case _ => None
      }
    }

    object ModsPlaceholder {
      def unapply(tree: Tree): Option[String] = tree match {
        case q"new ${Ident(tpnme.QUASIQUOTE_MODS)}(${Literal(Constant(s: String))})" =>
          Some(s)
        case _ =>
          None
      }
    }

    def quasiquoteReify(tree: Tree): Tree = {
      val reified = reifyTree(tree)
      (placeholders.keys.toSet -- placeholders.accessed).foreach { hole =>
        c.abort(placeholders(hole)._1.pos, "Can't splice an instance of ${tree.tpe} in this position")
      }
      reified
    }

    override def reifyTree(tree: Tree): Tree = reifyBasicTree(tree)

    // TODO: make sure that this list is complete
    final val inlineFlags = List(
      PRIVATE, PROTECTED, LAZY, IMPLICIT,
      CASE, FINAL, COVARIANT, CONTRAVARIANT,
      OVERRIDE, SEALED)

    def requireNoInlineFlags(m: Modifiers, pos: Position, action: String) = {
      val flags = m.flags
      inlineFlags.foreach { f =>
        require((flags & f) == 0L, pos, "Can't $action Modifiers together with inline Flags.")
      }
    }

    override def mirrorSelect(name: String): Tree =
      Select(universe, TermName(name))

    override def mirrorCall(name: TermName, args: Tree*): Tree =
      Apply(Select(universe, name), args.toList)

    override def mirrorBuildCall(name: TermName, args: Tree*): Tree =
      Apply(Select(Select(universe, nme.build), name), args.toList)
  }

  class ApplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {
    def isSupportedZeroCardinalityType(tpe: Type): Boolean =
      tpe <:< treeType || tpe <:< nameType || tpe <:< modsType || tpe <:< flagsType

    object CorrespondsTo {

      def unapply(name: Name): Option[(Tree, Type)] = unapply(name.toString)

      def unapply(name: String): Option[(Tree, Type)] =
        placeholders.get(name).flatMap { case (tree, card) =>
          (card, tree.tpe) match {
            case (0, tpe) if isSupportedZeroCardinalityType(tpe) =>
              Some((tree, tpe))
            case (0, LiftableType(lift)) =>
              Some((wrapLift(lift, tree), treeType))
            case (card, iterable) if card > 0 && iterable <:< iterableType =>
              extractIterableN(card, iterable).map {
                case tpe if tpe <:< treeType =>
                  if (iterable <:< listTreeType || iterable <:< listListTreeType) {
                    Some(tree, iterable)
                  } else {
                    Some((wrapIterableN(tree, card) { t => t }, iterableN(card, tpe)))
                  }
                case LiftableType(lift) =>
                  Some((wrapIterableN(tree, card) { t => wrapLift(lift, t) }, iterableN(card, treeType)))
                case tpe =>
                  c.abort(tree.pos, s"Can't splice an Iterable of non-liftable type $tpe.")
              }.getOrElse {
                c.abort(tree.pos, s"Incorrect cardinality.")
              }
            case (card, tpe) =>
              c.abort(tree.pos, s"Splicing of type $tpe with '${fmtCard(card)}' cardinality isn't supported.")
          }
        }

      def wrapLift(lift: Tree, tree: Tree) =
        q"$lift($u, $tree).asInstanceOf[$u.Tree]"

      def wrapIterableN(tree: Tree, n: Int)(default: Tree => Tree): Tree = n match {
        case 0 => default(tree)
        case _ =>
          val x: TermName = c.freshName()
          val wrapped = wrapIterableN(Ident(x), n - 1)(default)
          q"$tree.map { $x => $wrapped }.toList"
      }

      object LiftableType {
        def unapply(tpe: Type): Option[Tree] = {
          val liftType = appliedType(liftableType, List(tpe))
          val lift = c.inferImplicitValue(liftType, silent = true)
          if (lift != EmptyTree)
            Some(lift)
          else
            None
        }
      }

      def iterableN(n: Int, tpe: Type): Type =
        if (n == 0) tpe
        else appliedType(IterableClass.toType, List(iterableN(n - 1, tpe)))

      def extractIterableN(n: Int, tpe: Type): Option[Type] =
        if (n == 0) Some(tpe)
        else if (tpe <:< iterableType) extractIterableN(n - 1, tpe.typeArguments(0))
        else None
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case Literal(Constant(true)) => q"$u.build.True"
      case Literal(Constant(false)) => q"$u.build.False"
      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< treeType => tree
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE_TYPE), args) => reifyTupleType(args)
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) => reifyTuple(args)
      case Apply(f, List(Placeholder(CorrespondsTo(argss, tpe)))) if tpe <:< iterableIterableTreeType =>
        val f1 = reifyTree(f)
        q"$argss.foldLeft[$u.Tree]($f1) { $u.Apply(_, _) }"
      case Block(stats, p @ Placeholder(CorrespondsTo(tree, tpe))) =>
        mirrorBuildCall("Block", reifyList(stats :+ p))
      case SyntacticalClassDef(mods, name, tparams, constrmods, argss, parents, selfval, body) =>
        mirrorBuildCall("SyntacticalClassDef", reifyModifiers(mods), reifyName(name),
                        reifyList(tparams), reifyModifiers(constrmods), reifyList(argss),
                        reifyList(parents), reifyTree(selfval), reifyList(body))
      case Placeholder(name) if placeholders(name)._2 > 0 =>
        val (tree, card) = placeholders(name)
        c.abort(tree.pos, s"Can't splice tree with '${fmtCard(card)}' cardinality in this position.")
      case _ =>
        super.reifyBasicTree(tree)
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(q"()")
      case List(hole @ Placeholder(CorrespondsTo(tree, tpe))) if !(tpe <:< iterableType) => reify(hole)
      case List(Placeholder(_)) => mirrorBuildCall("TupleN", reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall("TupleN", reifyList(args))
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(tq"scala.Unit")
      case List(hole @ Placeholder(CorrespondsTo(tree, tpe))) if !(tpe <:< iterableType) => reify(hole)
      case List(Placeholder(_)) => mirrorBuildCall("TupleTypeN", reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall("TupleTypeN", reifyList(args))
    }

    override def reifyName(name: Name): Tree = name match {
      case CorrespondsTo(tree, tpe) =>
        if (tpe <:< nameType)
          tree
        else
          c.abort(tree.pos, s"Name expected but ${tpe} found.")
      case _ =>
        super.reifyName(name)
    }

    /** Splits list into a list of groups where subsequent elements are condidered
     *  similar by the corresponding function.
     *
     *  For example:
     *
     *  > group(List(1, 1, 0, 0, 1, 0)) { _ == _ }
     *  List(List(1, 1), List(0, 0), List(1), List(0))
     *
     */
    def group[T](lst: List[T])(similar: (T, T) => Boolean) = lst.foldLeft[List[List[T]]](List()) {
      case (Nil, el) => List(List(el))
      case (ll :+ (last @ (lastinit :+ lastel)), el) if similar(lastel, el) => ll :+ (last :+ el)
      case (ll, el) => ll :+ List(el)
    }

    /** Reifies list filling all the valid placeholders.
     *
     *  Reification of non-trivial list is done in two steps:
     *  1. split the list into groups where every placeholder is always
     *     put in a group of it's own and all subsquent non-placeholders are
     *     grouped together; element is considered to be a placeholder if it's
     *     in the domain of the fill function;
     *  2. fold the groups into a sequence of lists added together with ++ using
     *     fill reification for placeholders and fallback reification for non-placeholders.
     */
    def reifyListGeneric[T](xs: List[T])(fill: PartialFunction[T, Tree])(fallback: T => Tree): Tree =
      xs match {
        case Nil => mkList(Nil)
        case _ =>
          def reifyGroup(group: List[T]): Tree = group match {
            case List(elem) if fill.isDefinedAt(elem) => fill(elem)
            case elems => mkList(elems.map(fallback))
          }
          val head :: tail = group(xs) { (a, b) => !fill.isDefinedAt(a) && !fill.isDefinedAt(b) }
          tail.foldLeft[Tree](reifyGroup(head)) { (tree, lst) => q"$tree ++ ${reifyGroup(lst)}" }
      }

    /** Reifies arbitrary list filling ..$x and ...$y placeholders when they are put
     *  in the correct position. Fallbacks to super.reifyList for non-placeholders.
     */
    override def reifyList(xs: List[Any]): Tree = reifyListGeneric(xs) {
      case Placeholder(CorrespondsTo(tree, tpe)) if tpe <:< iterableTreeType => tree
      case List(Placeholder(CorrespondsTo(tree, tpe))) if tpe <:< iterableIterableTreeType => tree
    } {
      reify(_)
    }

    /** Custom list reifier for annotations. It's required because they have different shape
     *  and additional $u.build.annotationRepr wrapping is needed to ensure that user won't
     *  splice a non-constructor call in this position.
     */
    def reifyAnnotsList(annots: List[Tree]): Tree = reifyListGeneric(annots) {
      case AnnotPlaceholder(CorrespondsTo(tree, tpe), args) if tpe <:< iterableTreeType =>
        val x: TermName = c.freshName()
        q"$tree.map { $x => $u.build.annotationRepr($x, ${reify(args)}) }"
    } {
      case AnnotPlaceholder(CorrespondsTo(tree, tpe), args) if tpe <:< treeType =>
        q"$u.build.annotationRepr($tree, ${reify(args)})"
      case other => reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val (modsholes, annots) = m.annotations.partition {
        case ModsPlaceholder(_) => true
        case _ => false
      }
      val (mods, flags) = modsholes.map {
        case ModsPlaceholder(CorrespondsTo((tree, tpe))) => (tree, tpe)
      }.partition { case (tree, tpe) =>
        if (tpe <:< modsType)
          true
        else if (tpe <:< flagsType)
          false
        else
          c.abort(tree.pos, "Intance of FlagSet or Modifiers type is expected here but ${tree.tpe} found")
      }
      if (mods.nonEmpty) {
        val (tree, tpe) = mods(0)
        require(mods.length == 1, mods(1)._1.pos, "Can't splice multiple Modifiers")
        require(flags.isEmpty, flags(0)._1.pos, "Can't splice Flags together with Modifiers")
        require(annots.isEmpty, tree.pos, "Can't splice Modifiers together with additional annotations")
        requireNoInlineFlags(m, tree.pos, "splice")
        tree
      } else {
        val baseFlags = mirrorBuildCall(nme.flagsFromBits, reify(m.flags))
        val reifiedFlags = flags.foldLeft[Tree](baseFlags) { case (flag, (tree, _)) => q"$flag | $tree" }
        mirrorFactoryCall(nme.Modifiers, reifiedFlags, reify(m.privateWithin), reifyAnnotsList(annots))
      }
    }
  }

  class ApplyReifierWithSymbolSplicing(universe: Tree, placeholders: Placeholders) extends ApplyReifier(universe, placeholders) {
    override def isSupportedZeroCardinalityType(tpe: Type) =
      super.isSupportedZeroCardinalityType(tpe) || tpe <:< symbolType

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case Ident(PlaceholderName(CorrespondsTo(sym, tpe))) if tpe <:< symbolType =>
        q"$u.Ident($sym)"
      case Select(tree, PlaceholderName(CorrespondsTo(sym, tpe))) if tpe <:< symbolType =>
        q"$u.Select(${reifyTree(tree)}, $sym)"
      case _ =>
        super.reifyBasicTree(tree)
    }
  }

  class UnapplyReifier(universe: Tree, placeholders: Placeholders) extends Reifier(universe, placeholders) {
    object CorrespondsTo {
      def unapply(name: String): Option[(Tree, Int)] =
        placeholders.get(name)
    }

    override def reifyBasicTree(tree: Tree): Tree = tree match {
      case global.emptyValDef =>
        mirrorBuildCall("EmptyValDefLike")
      case global.pendingSuperCall =>
        mirrorBuildCall("PendingSuperCallLike")
      case Placeholder(CorrespondsTo(tree, card)) =>
        if (card > 0)
          c.abort(tree.pos, s"Can't extract a part of the tree with '${fmtCard(card)}' cardinality in this position.")
        tree
      case AppliedTypeTree(Ident(tpnme.QUASIQUOTE_TUPLE_TYPE), args) => reifyTupleType(args)
      case Apply(Ident(nme.QUASIQUOTE_TUPLE), args) => reifyTuple(args)
      case Applied(fun, targs, argss) if fun != tree =>
        if (targs.length > 0)
          mirrorBuildCall("Applied", reify(fun), reifyList(targs), reifyList(argss))
        else
          mirrorBuildCall("Applied2", reify(fun), reifyList(argss))
      case SyntacticalClassDef(mods, name, tparams, constrmods, argss, parents, selfval, body) =>
        mirrorBuildCall("SyntacticalClassDef", reifyModifiers(mods), reifyName(name),
                        reifyList(tparams), reifyModifiers(constrmods), reifyList(argss),
                        reifyList(parents), reifyTree(selfval), reifyList(body))
      case _ =>
        super.reifyBasicTree(tree)
    }

    def reifyTuple(args: List[Tree]) = args match {
      case Nil => reify(q"()")
      case List(hole @ Placeholder(CorrespondsTo(tree, 0))) => reify(hole)
      case List(Placeholder(CorrespondsTo(tree, card))) => mirrorBuildCall("TupleN", reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall("TupleN", reifyList(args))
    }

    def reifyTupleType(args: List[Tree]) = args match {
      case Nil => reify(tq"scala.Unit")
      case List(hole @ Placeholder(CorrespondsTo(tree, 0))) => reify(hole)
      case List(Placeholder(CorrespondsTo(tree, card))) => mirrorBuildCall("TupleTypeN", reifyList(args))
      case List(other) => reify(other)
      case _ => mirrorBuildCall("TupleTypeN", reifyList(args))
    }

    override def scalaFactoryCall(name: String, args: Tree*): Tree =
      call("scala." + name, args: _*)

    override def reifyName(name: Name): Tree =
      if (!placeholders.contains(name.toString))
        super.reifyName(name)
      else {
        placeholders(name.toString)._1
      }

    def reifyListGeneric(xs: List[Any])(fill: PartialFunction[Any, Tree])(fallback: Any => Tree) =
      xs match {
        case init :+ last if fill.isDefinedAt(last) =>
          init.foldRight[Tree](fill(last)) { (el, rest) =>
            q"scala.collection.immutable.$$colon$$colon(${fallback(el)}, $rest)"
          }
        case _ =>
          mkList(xs.map(fallback))
      }

    override def reifyList(xs: List[Any]): Tree = reifyListGeneric(xs) {
      case Placeholder(CorrespondsTo(tree, 1)) => tree
      case List(Placeholder(CorrespondsTo(tree, 2))) => tree
    } {
      reify _
    }

    def reifyAnnotsList(annots: List[Tree]): Tree = reifyListGeneric(annots) {
      case AnnotPlaceholder(CorrespondsTo(tree, 1), Nil) => tree
    } {
      case AnnotPlaceholder(CorrespondsTo(tree, 0), args) =>
        args match {
          case Nil => tree
          case _ => q"$u.Apply($u.Select($u.New($tree), $u.nme.CONSTRUCTOR), ${reify(args)})"
        }
      case other =>
        reify(other)
    }

    override def reifyModifiers(m: Modifiers) = {
      val mods = m.annotations.collect {
        case ModsPlaceholder(CorrespondsTo(tree, _)) => tree
      }
      if (mods.nonEmpty) {
        val tree = mods(0)
        require(mods.length == 1, mods(1).pos, "Can't extract multiple Modifiers")
        require(m.annotations.length == 1, tree.pos, "Can't extract Modifiers together with additional annotations")
        requireNoInlineFlags(m, tree.pos, "extract")
        tree
      } else
        mirrorFactoryCall(nme.Modifiers, mirrorBuildCall("FlagsAsBits", reify(m.flags)),
                                         reify(m.privateWithin), reifyAnnotsList(m.annotations))
    }
  }

  trait Types {
    val universe: Tree

    lazy val universeType = universe.tpe
    lazy val nameType = memberType(universeType, tpnme.Name)
    lazy val termNameType = memberType(universeType, tpnme.TypeName)
    lazy val typeNameType = memberType(universeType, tpnme.TermName)
    lazy val modsType = memberType(universeType, tpnme.Modifiers)
    lazy val flagsType = memberType(universeType, tpnme.FlagSet)
    lazy val symbolType = memberType(universeType, tpnme.Symbol)
    lazy val treeType = memberType(universeType, tpnme.Tree)
    lazy val typeDefType = memberType(universeType, tpnme.TypeDef)
    lazy val liftableType = LiftableClass.toType
    lazy val iterableType = appliedType(IterableClass.toType, List(AnyTpe))
    lazy val iterableTreeType = appliedType(iterableType, List(treeType))
    lazy val iterableIterableTreeType = appliedType(iterableType, List(iterableTreeType))
    lazy val listType = appliedType(ListClass.toType, List(AnyTpe))
    lazy val listTreeType = appliedType(listType, List(treeType))
    lazy val listListTreeType = appliedType(listType, List(listTreeType))
    lazy val optionTreeType = appliedType(OptionClass.toType, List(treeType))
    lazy val optionNameType = appliedType(OptionClass.toType, List(nameType))
  }

  def memberType(thistype: Type, name: TypeName): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(name)
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }

  def fmtCard(cardinality: Int) =
    if (cardinality == 0)
      ""
    else
      "." * (cardinality + 1)
}