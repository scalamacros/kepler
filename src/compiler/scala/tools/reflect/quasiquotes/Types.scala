package scala.tools.reflect
package quasiquotes

import scala.tools.nsc.Global

trait Types {

  val global: Global
  val universe: global.Tree
  import global._
  import global.definitions._

  lazy val universeType = universe.tpe
  lazy val nameType = memberType(universeType, "Name")
  lazy val termNameType = memberType(universeType, "TypeName")
  lazy val typeNameType = memberType(universeType, "TermName")
  lazy val treeType = memberType(universeType, "Tree")
  lazy val typeDefType = memberType(universeType, "TypeDef")
  lazy val liftableType = rootMirror.staticClass("scala.reflect.api.Liftable").toType
  lazy val listTreeType = appliedType(ListClass.toType, List(treeType))
  lazy val listListTreeType = appliedType(ListClass.toType, List(listTreeType))
  lazy val typeDefListType = appliedType(ListClass.toType, List(typeDefType))
  lazy val optionTreeType = appliedType(OptionClass.toType, List(treeType))
  lazy val optionNameType = appliedType(OptionClass.toType, List(nameType))

  def memberType(thistype: Type, name: String): Type = {
    val sym = thistype.typeSymbol.typeSignature.member(newTypeName(name))
    sym.asType.toType.typeConstructor.asSeenFrom(thistype, sym.owner)
  }
}
