import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermDeconstructionProps extends QuasiquoteProperties("term deconstruction") {

  property("f(x)") = forAll { (x: Tree) =>
    val q"f($x1)" = q"f($x)"
    x1 ≈ x
  }

  property("f(..xs)") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(..$xs)" = q"f($x1, $x2)"
    xs ≈ List(x1, x2)
  }

  property("f(y, ..ys)") = forAll { (x1: Tree, x2: Tree, x3: Tree) =>
    val q"f($y, ..$ys)" = q"f($x1, $x2, $x3)"
    y ≈ x1 && ys ≈ List(x2, x3)
  }

  property("f(y1, y2, ..ys)") = forAll { (x1: Tree, x2: Tree, x3: Tree) =>
    val q"f($y1, $y2, ..$ys)" = q"f($x1, $x2, $x3)"
    y1 ≈ x1 && y2 ≈ x2 && ys ≈ List(x3)
  }

  property("f(...xss)") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(...$argss)" = q"f($x1)($x2)"
    argss ≈ List(List(x1), List(x2))
  }

  // property("@$annot def foo") = forAll { (annotName: TypeName) =>
  //   val q"@$annot def foo" = q"@$annotName def foo"
  //   println(showRaw(annot))
  //   annot ≈ Apply(Select(New(Ident(annotName)), nme.CONSTRUCTOR), List())
  // }

}