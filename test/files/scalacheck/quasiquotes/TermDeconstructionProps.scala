import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermDeconstructionProps extends QuasiquoteProperties("term deconstruction")
                                  with ApplicationDeconstructionProps
                                  with AnnotationDeconstructionProps
                                  with ClassProps

trait ApplicationDeconstructionProps { self: QuasiquoteProperties =>

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
}

trait AnnotationDeconstructionProps extends AnnotationConstr { self: TermDeconstructionProps.type =>

  property("@$annot def foo") = forAll { (annotName: TypeName) =>
    val q"@$annot def foo" = q"@$annotName def foo"
    annot ≈ Apply(Select(New(Ident(annotName)), nme.CONSTRUCTOR), List())
  }

  property("@$annot(..$args) def foo") = forAll { (annotName: TypeName, tree: Tree) =>
    val q"@$annot(..$args) def foo" = q"@$annotName($tree) def foo"
    annot ≈ Ident(annotName) && args ≈ List(tree)
  }

  property("@..$annots def foo") = test {
    val a = annot("a")
    val b = annot("b")
    val q"@..$annots def foo" = q"@$a @$b def foo"
    annots ≈ List(a, b)
  }

  property("@$annot @..$annots def foo") = test {
    val a = annot("a")
    val b = annot("b")
    val c = annot("c")
    val q"@$first @..$rest def foo" = q"@$a @$b @$c def foo"
    first ≈ a && rest ≈ List(b, c)
  }
}

trait ClassProps { self: QuasiquoteProperties =>

  property("class without params") = test {
    val q"class $name { ..$body }" = q"class Foo { def bar = 3 }"
    assert(body ≈ List(q"def bar = 3"))
  }

  property("class constructor") = test {
    val q"class $name(...$argss)" = q"class Foo(x: Int)(y: Int)"
    assert(argss.length == 2)
  }

  property("class parents") = test {
    val q"class $name extends ..$parents" = q"class Foo extends Bar with Blah"
    assert(parents ≈ List(tq"Bar", tq"Blah"))
  }

  property("class selfdef") = test {
    val q"class $name { $self => }" = q"class Foo { self: T => }"
    assert(self.name ≈ TermName("self") && self.tpt ≈ tq"T")
  }

  property("class tparams") = test {
    val q"class $name[..$tparams]" = q"class Foo[A, B]"
    assert(tparams.map { _.name } == List(TypeName("A"), TypeName("B")))

  }

  // TODO: FIX ME
  // property("deconstruct early val defs") = test {
  //   val q"new { ..$defs } with $bla " = q"new { val x = 0 } with Foo"
  // }
}