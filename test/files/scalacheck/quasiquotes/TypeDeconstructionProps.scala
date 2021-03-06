import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TypeDeconstructionProps extends QuasiquoteProperties("type deconstruction") {
  property("ident(type name)") = forAll { (name: TypeName) =>
    val t = Ident(name)
    val tq"$t1" = t
    t1 ≈ t
  }

  property("applied type tree") = forAll { (name1: TypeName, name2: TypeName) =>
    val tq"$a[$b]" = AppliedTypeTree(Ident(name1), List(Ident(name2)))
    a ≈ Ident(name1) && b ≈ Ident(name2)
  }

  property("dependent type tree single arg") = forAll { (name: TypeName, arg: Tree) =>
    val tq"$name1($arg1)" = DependentTypeTree(Ident(name), List(arg))
    name1 ≈ Ident(name) && arg1 ≈ arg
  }

  property("dependent type tree multiple args") = forAll { (name: TypeName, args: List[Tree]) =>
    val tq"$name1(..$args1)" = DependentTypeTree(Ident(name), args)
    name1 ≈ Ident(name) && args1.zip(args).forall { case (a1, a) => a1 ≈ a }
  }

  property("tuple type") = test {
    val tq"(..$empty)" = tq"scala.Unit"
    assert(empty.isEmpty)
    val tq"(..$ts)" = tq"(t1, t2)"
    assert(ts ≈ List(tq"t1", tq"t2"))
    val tq"($head, ..$tail)" = tq"(t0, t1, t2)"
    assert(head ≈ tq"t0" && tail ≈ List(tq"t1", tq"t2"))
  }
}