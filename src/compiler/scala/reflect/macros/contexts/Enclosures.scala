package scala.reflect.macros
package contexts

import scala.reflect.{ClassTag, classTag}

trait Enclosures {
  self: Context =>

  import universe._

  type MacroRole = analyzer.MacroRole
  def APPLY_ROLE = analyzer.APPLY_ROLE
  def TYPE_ROLE = analyzer.TYPE_ROLE
  def APPLIED_TYPE_ROLE = analyzer.APPLIED_TYPE_ROLE
  def PARENT_ROLE = analyzer.PARENT_ROLE
  def NEW_ROLE = analyzer.NEW_ROLE
  def ANNOTATION_ROLE = analyzer.ANNOTATION_ROLE
  def macroRole: MacroRole

  private lazy val site       = callsiteTyper.context
  private lazy val enclTrees  = site.enclosingContextChain map (_.tree)
  private lazy val enclPoses  = enclosingMacros map (_.macroApplication.pos) filterNot (_ eq NoPosition)

  private def lenientEnclosure[T <: Tree : ClassTag]: Tree = enclTrees collectFirst { case x: T => x } getOrElse EmptyTree
  private def strictEnclosure[T <: Tree : ClassTag]: T = enclTrees collectFirst { case x: T => x } getOrElse (throw new EnclosureException(classTag[T].runtimeClass, enclTrees))

  // vals are eager to simplify debugging
  // after all we wouldn't save that much time by making them lazy
  val macroApplication: Tree                      = expandee
  def enclosingPackage: PackageDef                = strictEnclosure[PackageDef]
  val enclosingClass: Tree                        = lenientEnclosure[ImplDef]
  def enclosingImpl: ImplDef                      = strictEnclosure[ImplDef]
  def enclosingTemplate: Template                 = analyzer.macroExpanderAttachment(expandee).enclosingTemplate getOrElse strictEnclosure[Template]
  val enclosingImplicits: List[ImplicitCandidate] = site.openImplicits.map(_.toImplicitCandidate)
  val enclosingMacros: List[Context]              = this :: universe.analyzer.openMacros // include self
  val enclosingMethod: Tree                       = lenientEnclosure[DefDef]
  def enclosingDef: DefDef                        = strictEnclosure[DefDef]
  val enclosingPosition: Position                 = if (enclPoses.isEmpty) NoPosition else enclPoses.head.pos
  val enclosingUnit: CompilationUnit              = universe.currentRun.currentUnit
  val enclosingRun: Run                           = universe.currentRun
}
