package org.scalacheck

import language.experimental.macros
import scala.reflect.macros._

object fails {
  def apply(error: String)(code: _) = macro impl
  def impl(c: Context)(error: c.Expr[String])(code: c.Tree) = {
    import c.universe._
    def result(ok: Boolean, description: String = "") = {
      val status =
        if(ok)
          q"org.scalacheck.Prop.Proof"
        else
          q"org.scalacheck.Prop.False"
      val labels =
        if (description != "")
          q"scala.collection.immutable.Set($description)"
        else
          q"scala.collection.immutable.Set.empty[String]"
      q"""org.scalacheck.Prop {
        new org.scalacheck.Prop.Result(
          $status,
          Nil,
          scala.collection.immutable.Set.empty[scala.Any],
          $labels
        )
      }"""
    }
    try {
      c.typeCheck(code)
      result(false, "given code doesn't fail to typecheck")
    } catch {
      case e: TypecheckException =>
        val expected = c.eval(error)
        if (e.msg != expected )
          result(false, s"error message '${e.msg}' is not the same as expected '$expected'")
        else
          result(true)
    }
  }
}