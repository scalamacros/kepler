import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TypeConstructionProps extends Properties("type construction")
                                with TreeSimiliarity
                                with ArbitraryTreesAndNames {

  property("bare idents contain type names") = exists { (u: Unit) =>
    tq"x" ≈ Ident(TypeName("x"))
  }

  property("splice type names into AppliedTypeTree") = forAll { (name1: TypeName, name2: TypeName) =>
    tq"$name1[$name2]" ≈ AppliedTypeTree(Ident(name1), List(Ident(name2)))
  }

}