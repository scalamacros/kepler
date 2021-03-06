import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    val name = TypeName("C_" + c.weakTypeOf[T].toString + "_" + x.tree.toString)
    c.topLevelRef(name) orElse {
      val Block(List(dummy: ClassDef), _) = reify{ class DUMMY }.tree
      val synthetic = ClassDef(NoMods, name, Nil, dummy.impl)
      c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, synthetic)
    }
  }
}