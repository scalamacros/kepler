/*
    Shortest Test
*/
import scala.reflect.macros.{Context => Ctx}

object Test {
    def main(args: Array[String]): Unit = {
        println(A1.foo(42))
        //A1.foo2(43)
    }
}

object A1 {
    def impl(c: Ctx)(x: c.Expr[Int]) = {
        import c.universe._
        val a = 1
        val b = 2
        val k = foo2(10)
        x //reify { println( x.splice + O.O.k ) }
    }
    def foo(x: Int) = macro A1.impl

    def impl2(c: Ctx)(x: c.Expr[Int]):c.Expr[Int] = {
        import c.universe._
        val a = 1
        val b = 2
        x // reify { println( x.splice + O.O.k ) }
    }
    def foo2(x: Int) = macro A1.impl2
}


object O {
    object O {
        def k = 1
    }
}
