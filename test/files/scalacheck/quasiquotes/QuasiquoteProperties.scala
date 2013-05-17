import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._

class QuasiquoteProperties(name: String) extends Properties(name) with ArbitraryTreesAndNames {
  /** Runs a code block and returns proof confirmation
   *  if no exception has been thrown while executing code
   *  block. This is useful for simple one-off tests.
   */
  def test[T](block: => T)=
    Prop { (params) =>
      block
      Result(Prop.Proof)
    }

  implicit class TestSimilarTree(tree1: Tree) {
    def ≈(tree2: Tree) = tree1.equalsStructure(tree2)
  }

  implicit class TestSimilarListTree(lst: List[Tree]) {
    def ≈(other: List[Tree]) = (lst.length == other.length) && lst.zip(other).forall { case (t1, t2) => t1 ≈ t2 }
  }

  implicit class TestSimilarListListTree(lst: List[List[Tree]]) {
    def ≈(other: List[List[Tree]]) = (lst.length == other.length) && lst.zip(other).forall { case (l1, l2) => l1 ≈ l2 }
  }

  implicit class TestSimilarName(name: Name) {
    def ≈(other: Name) = name == other
  }

  def assertThrows[T <: AnyRef](f: => Any)(implicit manifest: Manifest[T]): Unit = {
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    val thrown =
      try {
        f
        false
      } catch {
        case u: Throwable =>
          if (!clazz.isAssignableFrom(u.getClass))
            assert(false, s"wrong exception: $u")
          true
      }
    if(!thrown)
      assert(false, "exception wasn't thrown")
  }

  def annot(name: String): Tree = annot(TypeName(name), Nil)
  def annot(name: TypeName): Tree = annot(name, Nil)
  def annot(name: String, args: List[Tree]): Tree = annot(TypeName(name), args)
  def annot(name: TypeName, args: List[Tree]): Tree = q"new $name(..$args)"
}

