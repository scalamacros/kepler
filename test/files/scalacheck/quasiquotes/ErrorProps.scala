import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object ErrorProps extends QuasiquoteProperties("errors") {
  // // This test fails due to bug in untyped macro expansion
  // property("deconstruction: can't use two '..' cardinalities in a row") = fails (
  //   "Can't extract a part of the tree with '..' cardinality in this position."
  // ) {
  //   val xs = List(q"x1", q"x2")
  //   val q"f(..$xs1, ..$xs2)" = xs
  // }

  property("can't splice with given cardinality") = fails (
    "Splicing of type List[reflect.runtime.universe.Ident] with '' cardinality isn't supported."
  ) {
    val xs = List(q"x", q"x")
    q"$xs"
  }

  property("splice typename into typedef with default bounds") = fails (
    "Name expected but reflect.runtime.universe.TypeDef found."
  ) {
    val T1 = TypeName("T1")
    val T2 = q"type T"
    val t = EmptyTree
    q"type $T1[$T2 >: _root_.scala.Any <: _root_.scala.Nothing] = $t" ≈
      TypeDef(Modifiers(), T1, List(T2), t)
  }

  property("can't splice annotations with '...' cardinality") = fails (
    "Can't splice tree with '...' cardinality in this position."
  ) {
    val annots = List(List(q"Foo"))
    q"@...$annots def foo"
  }

  // // This test fails due to bug in untyped macro expansion
  // property("@..$first @$rest def foo") = fails (
  //   "Can't extract a part of the tree with '..' cardinality in this position."
  // ) {
  //   val a = annot("a")
  //   val b = annot("b")
  //   val c = annot("c")
  //   val q"@..$first @$rest def foo" = q"@$a @$b @$c def foo"
  // }

  // // Make sure a nice error is reported in this case
  // { import Flag._; val mods = NoMods; q"lazy $mods val x: Int" }
}