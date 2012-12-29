package scala.reflect
package api

trait QuasiQuotes { self: Universe =>

  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.ApplyQMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def apply(args: Any*): Any = ??? // macro
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.UnapplyQMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def unapply(tree: Any): Option[Any] = ??? // macro
    }
    object tq {
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.ApplyTQMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def apply(args: Any*): Any = ??? // macro
      // implementation is hardwired to `scala.tools.reflect.quasiquotes.UnapplyTQMacro`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def unapply(tree: Any): Option[Any] = ??? // macro
    }
  }

  implicit object liftType extends Liftable[Type] {
    def apply(universe: Universe, value: Type): universe.Tree = {
      require(universe eq self, "Can't lift Type from different universe.")
      universe.TypeTree(value.asInstanceOf[universe.Type])
    }
  }

  implicit object liftSymbol extends Liftable[Symbol] {
    def apply(universe: Universe, value: Symbol): universe.Tree = {
      require(universe eq self, "Can't lift Symbol from different universe.")
      universe.Ident(value.asInstanceOf[universe.Symbol])
    }
  }
}

case class QuasiQuoteException(msg: String) extends Exception(msg)
