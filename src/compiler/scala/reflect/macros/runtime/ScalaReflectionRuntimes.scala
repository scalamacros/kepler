package scala.reflect.macros
package runtime

import scala.reflect.internal.Flags._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.ReflectionUtils

trait ScalaReflectionRuntimes {
  self: scala.tools.nsc.typechecker.Analyzer =>

  trait ScalaReflectionResolvers {
    self: MacroRuntimeResolver =>

    import global._

    def resolveScalaReflectionRuntime(classLoader: ClassLoader): MacroRuntime = {
      macroLogVerbose(s"[scala reflection resolver] resolved implementation as $className.$methName")

      try {
        macroTraceVerbose("loading implementation class: ")(className)
        macroTraceVerbose("classloader is: ")(ReflectionUtils.show(classLoader))
        val macroMirror: ru.JavaMirror = ru.runtimeMirror(classLoader)
        val implContainerSym = macroMirror.classSymbol(Class.forName(className, true, classLoader))
        val implMethSym = implContainerSym.typeSignature.member(ru.TermName(methName)).asMethod
        macroLogVerbose(s"successfully loaded macro impl as ($implContainerSym, $implMethSym)")
        args => {
          val implContainer =
            if (isBundle) {
              val implCtorSym = implContainerSym.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod
              macroMirror.reflectClass(implContainerSym).reflectConstructor(implCtorSym)(args.c)
            } else {
              macroMirror.reflectModule(implContainerSym.module.asModule).instance
            }
          val implMeth = macroMirror.reflect(implContainer).reflectMethod(implMethSym)
          val implArgs = if (isBundle) args.others else args.c +: args.others
          implMeth(implArgs: _*)
        }
      } catch {
        case ex: Exception =>
          macroTraceVerbose(s"macro runtime failed to load: ")(ex.toString)
          if (flavor == FLAVOR_EXPAND) macroDef setFlag IS_ERROR
          null
      }
    }
  }
}
