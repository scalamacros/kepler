import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox
import scala.reflect.api._
import scala.reflect.mirror._
import scala.reflect.api.Modifier._
import scala.reflect.internal.Flags._

object Test extends App {
  //val code = scala.reflect.Code.lift{
  //  List(1).map(_ * 2)
  //};

  def list = Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("immutable")), newTermName("List"))
  val list_of_2 = Apply(Select(list, newTermName("apply")), List(Literal(Constant(2))))
  val map = Select(list_of_2, newTermName("map"))
  val fn = Function(
    List(ValDef(Modifiers(Set(parameter)), newTermName("x"), TypeTree(), EmptyTree)),
    Ident(newTermName("x")))
  val imp = Select(list, newTermName("canBuildFrom"))
  val inv = Apply(Apply(map, List(fn)), List(imp))
  //val inv = Apply(map, List(fn))
  val code = new { val tree = inv }

  val settings = new Settings
  settings.debug.value = true
  settings.Ytyperdebug.value = true

  val reporter = new ConsoleReporter(settings)
  val toolbox = new ToolBox(reporter)
//  val ttree = toolbox.typeCheck(code.tree)
  val run = new toolbox.compiler.Run
  toolbox.compiler.phase = run.parserPhase
  var ttree = toolbox.importer.importTree(code.tree.asInstanceOf[toolbox.importer.from.Tree])
//  ttree = toolbox.compiler.typer.typed(ttree, toolbox.compiler.analyzer.EXPRmode, toolbox.compiler.WildcardType)
  println(toolbox.compiler.runExpr(ttree))
}
