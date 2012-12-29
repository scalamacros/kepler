import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TypeDeconstructionProps extends Properties("type deconstruction")
                                  with TreeSimiliarity
                                  with ArbitraryTreesAndNames {

  property("ident(type name)") = forAll { (name: TypeName) =>
    val t = Ident(name)
    val tq"$t1" = t
    t1 ≈ t
  }

  property("applied type tree") = forAll { (name1: TypeName, name2: TypeName) =>
    val tq"$a[$b]" = AppliedTypeTree(Ident(name1), List(Ident(name2)))
    a ≈ Ident(name1) && b ≈ Ident(name2)
  }

}