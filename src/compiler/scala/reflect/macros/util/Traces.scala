package scala.reflect.macros
package util

trait Traces {
  def globalSettings: scala.tools.nsc.Settings

  val macroDebugLite = globalSettings.YmacrodebugLite.value
  val macroDebugVerbose = globalSettings.YmacrodebugVerbose.value
  val macroDebugExpand  = globalSettings.YmacrodebugExpand.value
  val macroTraceLite = scala.tools.nsc.util.trace when (macroDebugLite || macroDebugVerbose)
  val macroTraceVerbose = scala.tools.nsc.util.trace when macroDebugVerbose
  val macroTraceExpand = scala.tools.nsc.util.trace when macroDebugExpand
  @inline final def macroLogLite(msg: => Any) { if (macroDebugLite || macroDebugVerbose) println(msg) }
  @inline final def macroLogVerbose(msg: => Any) { if (macroDebugVerbose) println(msg) }
  @inline final def macroLogExpand(msg: => Any) { if (macroDebugExpand) println(msg) }
}
