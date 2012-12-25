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
    }
  }
}

case class QuasiQuoteException(msg: String) extends Exception(msg)
