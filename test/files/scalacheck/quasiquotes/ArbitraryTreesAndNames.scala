import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.Flag._


trait ArbitraryTreesAndNames {

  def smallList[T](size: Int, g: Gen[T]) = {
    val n: Int = choose(0, size / 2 + 1).sample match {
      case Some(i) => i
      case None => 0
    }
    containerOfN[List, T](n, g)
  }

  def shortIdent(len: Int) =
    for(name <- identifier)
      yield
        if(name.length <= len)
          name
        else
          name.substring(0, len - 1)

  def genTermName = for(name <- shortIdent(8)) yield TermName(name)
  def genTypeName = for(name <- shortIdent(8)) yield TypeName(name)
  def genName = oneOf(genTermName, genTypeName)

  def genFlagSet = oneOf(
    TRAIT, INTERFACE, MUTABLE, MACRO,
    DEFERRED, ABSTRACT, FINAL, SEALED,
    IMPLICIT, LAZY, OVERRIDE, PRIVATE,
    PROTECTED, LOCAL, CASE, ABSOVERRIDE,
    BYNAMEPARAM, PARAM, COVARIANT, CONTRAVARIANT,
    DEFAULTPARAM, PRESUPER, DEFAULTINIT
  )

  def genModifiers = for(flagset <- genFlagSet) yield Modifiers(flagset)

  def genConstant =
    for(value <- oneOf(arbitrary[Byte], arbitrary[Short], arbitrary[Char],
                       arbitrary[Int], arbitrary[Long], arbitrary[Float],
                       arbitrary[Double], arbitrary[Boolean], arbitrary[String]))
      yield Constant(value)

  def genAnnotated(size: Int) =
    for(annot <- genTree(size - 1); arg <- genTree(size - 1))
      yield Annotated(annot, arg)

  def genAlternative(size: Int): Gen[Alternative] =
    for(trees <- smallList(size, genTree(size - 1)))
      yield Alternative(trees)

  def genAppliedTypeTree(size: Int) =
    for(tpt <- genTree(size - 1) if tpt.isType;
        args <- smallList(size, genTree(size - 1)))
      yield AppliedTypeTree(tpt, args)

  def genApply(size: Int) =
    for(fun <- genTree(size - 1);
        args <- smallList(size, genTree(size - 1)))
      yield Apply(fun, args)

  def genAssign(size: Int) =
    for(lhs <- genTree(size - 1); rhs <- genTree(size - 1))
      yield Assign(lhs, rhs)

  def genAssignOrNamedArg(size: Int) =
    for(lhs <- genTree(size - 1); rhs <- genTree(size - 1))
      yield AssignOrNamedArg(lhs, rhs)

  def genBind(size: Int) =
    for(name <- genName; body <- genTree(size - 1))
      yield Bind(name, body)

  def genBlock(size: Int) =
    for(stats <- smallList(size, genTree(size - 1)); expr <- genTree(size - 1))
      yield Block(stats, expr)

  def genCaseDef(size: Int) =
    for(pat <- genTree(size - 1); guard <- genTree(size - 1); body <- genTree(size - 1))
      yield CaseDef(pat, guard, body)

  def genClassDef(size: Int) =
    for(mods <- genModifiers; name <- genTypeName;
        tparams <- smallList(size, genTypeDef(size - 1));
        impl <- genTemplate(size - 1))
      yield ClassDef(mods, name, tparams, impl)

  def genCompoundTypeTree(size: Int) =
    for(templ <- genTemplate(size - 1))
      yield CompoundTypeTree(templ)

  def genDefDef(size: Int) =
    for(mods <- genModifiers; name <- genName;
        tpt <- genTree(size -1); rhs <- genTree(size - 1);
        tparams <- smallList(size, genTypeDef(size - 1));
        vparamss <- smallList(size, smallList(size, genValDef(size - 1))))
      yield DefDef(mods, name, tparams, vparamss, tpt, rhs)

  def genExistentialTypeTree(size: Int) =
    for(tpt <- genTree(size - 1); where <- smallList(size, genTree(size - 1)))
      yield ExistentialTypeTree(tpt, where)

  def genFunction(size: Int) =
    for(vparams <- smallList(size, genValDef(size - 1)); body <- genTree(size - 1))
      yield Function(vparams, body)

  def genIdent =
    for(name <- genName) yield Ident(name)

  def genIf(size: Int) =
    for(cond <- genTree(size - 1); thenp <- genTree(size - 1); elsep <- genTree(size - 1))
      yield If(cond, thenp, elsep)

  def genImport(size: Int) =
    for(expr <- genTree(size - 1); selectors <- smallList(size, genImportSelector(size - 1)))
      yield Import(expr, selectors)

  def genImportSelector(size: Int) =
    for(name <- genName; namePos <- arbitrary[Int]; rename <- genName; renamePos <- arbitrary[Int])
      yield ImportSelector(name, namePos, rename, renamePos)

  def genTemplate(size: Int) =
    for(parents <- smallList(size, genTree(size - 1));
        self <- genValDef(size - 1);
        body <- smallList(size, genTree(size - 1)))
      yield Template(parents, self, body)

  def genLabelDef(size: Int) =
    for(name <- genTermName; params <- smallList(size, genIdent); rhs <- genTree(size - 1))
      yield LabelDef(name, params, rhs)

  def genLiteral =
    for(const <- genConstant) yield Literal(const)

  def genMatch(size: Int) =
    for(selector <- genTree(size - 1); cases <- smallList(size, genCaseDef(size - 1)))
      yield Match(selector, cases)

  def genModuleDef(size: Int) =
    for(mods <- genModifiers; name <- genTermName; impl <- genTemplate(size - 1))
      yield ModuleDef(mods, name, impl)

  def genNew(size: Int) =
    for(tpt <- genTree(size - 1))
      yield New(tpt)

  def genRefTree(size: Int) =
    oneOf(genSelect(size), genIdent, genSelectFromTypeTree(size))

  def genPackageDef(size: Int) =
    for(reftree <- genRefTree(size - 1); stats <- smallList(size, genTree(size - 1)))
      yield PackageDef(reftree, stats)

  def genSelect(size: Int) =
    for(qual <- genTree(size - 1); name <- genTermName)
      yield Select(qual, name)

  def genSelectFromTypeTree(size: Int) =
    for(qual <- genTree(size - 1) if qual.isType; name <- genTypeName)
      yield SelectFromTypeTree(qual, name)

  def genReferenceToBoxed(size: Int) =
    for(ident <- genIdent)
      yield ReferenceToBoxed(ident)

  def genReturn(size: Int) =
    for(expr <- genTree(size - 1))
      yield Return(expr)

  def genSingletonTypeTree(size: Int) =
    for(expr <- genTree(size - 1))
      yield SingletonTypeTree(expr)

  def genStar(size: Int) =
    for(expr <- genTree(size - 1))
      yield Star(expr)

  def genSuper(size: Int) =
    for(qual <- genTree(size - 1); mix <- genTypeName)
      yield Super(qual, mix)

  def genThis(size: Int) =
    for(qual <- genTypeName)
      yield This(qual)

  def genThrow(size: Int) =
    for(expr <- genTree(size - 1))
      yield Throw(expr)

  def genTry(size: Int) =
    for(block <- genTree(size - 1);
        catches <- smallList(size, genCaseDef(size - 1));
        finalizer <- genTree(size - 1))
      yield Try(block, catches, finalizer)

  def genTypeApply(size: Int) =
    for(fun <- genTree(size - 1) if fun.isTerm; args <- smallList(size, genTree(size - 1)))
      yield TypeApply(fun, args)

  def genTypeBoundsTree(size: Int) =
    for(lo <- genTree(size - 1); hi <- genTree(size - 1))
      yield TypeBoundsTree(lo, hi)

  def genTypeDef(size: Int): Gen[TypeDef] =
    for(mods <- genModifiers; name <- genTypeName;
        tparams <- smallList(size, genTypeDef(size - 1)); rhs <- genTree(size - 1))
      yield TypeDef(mods, name, tparams, rhs)

  def genTypeTree: Gen[TypeTree] = TypeTree()

  def genTyped(size: Int) =
    for(expr <- genTree(size - 1); tpt <- genTree(size - 1))
      yield Typed(expr, tpt)

  def genUnApply(size: Int) =
    for(fun <- genTree(size - 1); args <- smallList(size, genTree(size - 1)))
      yield UnApply(fun, args)

  def genValDef(size: Int) =
    for(mods <- genModifiers; name <- genTermName;
        tpt <- genTree(size - 1); rhs <- genTree(size - 1))
      yield ValDef(mods, name, tpt, rhs)

  def genTree(size: Int): Gen[Tree] =
    if(size < 1)
      EmptyTree
    else if(size == 1)
      oneOf(genLiteral, genIdent, genTypeTree, EmptyTree)
    else
      oneOf(genTree(1), genValDef(size - 1), genTypeDef(size - 1),
            genAlternative(size - 1), genAnnotated(size - 1), genAppliedTypeTree(size - 1),
            genApply(size - 1), genAssign(size - 1), genAssignOrNamedArg(size - 1),
            genBind(size - 1), genBlock(size - 1), genCaseDef(size - 1),
            genClassDef(size - 1), genCompoundTypeTree(size - 1),
            genDefDef(size - 1), genExistentialTypeTree(size - 1),
            genFunction(size - 1), genIf(size - 1), genImport(size - 1),
            genLabelDef(size - 1), genMatch(size - 1), genModuleDef(size - 1),
            genNew(size - 1), genPackageDef(size - 1),
            genSelect(size - 1), genSelectFromTypeTree(size - 1),
            genReturn(size - 1), genSingletonTypeTree(size - 1),
            genTemplate(size - 1), genStar(size - 1), genSuper(size - 1),
            genThis(size - 1), genThrow(size - 1), genTry(size - 1),
            genTypeApply(size - 1), genTypeBoundsTree(size - 1),
            genTyped(size - 1), genUnApply(size - 1))

  // def genIdentIsType =
  //   for(name <- genTypeName) yield Ident(name)

  // def genTreeIsType(size: Int): Gen[Tree] =
  //   if(size < 1)
  //     EmptyTree
  //   else if(size == 1)
  //     oneOf(genIdentIsType, genTypeTree)
  //   else
  //     oneOf()

  implicit val arbConstant: Arbitrary[Constant] = Arbitrary(genConstant)
  implicit val arbModifiers: Arbitrary[Modifiers] = Arbitrary(genModifiers)
  implicit val arbTermName: Arbitrary[TermName] = Arbitrary(genTermName)
  implicit val arbTypeName: Arbitrary[TypeName] = Arbitrary(genTypeName)
  implicit val arbName: Arbitrary[Name] = Arbitrary(genName)

  implicit val arbLiteral: Arbitrary[Literal] = Arbitrary(genLiteral)
  implicit val arbIdent: Arbitrary[Ident] = Arbitrary(genIdent)
  implicit val arbValDef: Arbitrary[ValDef] = Arbitrary(sized(s => genValDef(5))) // TODO: hack
  implicit val arbDefDef: Arbitrary[DefDef] = Arbitrary(sized(s => genDefDef(5))) // TODO: hack
  implicit val arbTypeDef: Arbitrary[TypeDef] = Arbitrary(sized(s => genTypeDef(5))) // TODO: hack
  implicit val arbTree: Arbitrary[Tree] = Arbitrary(sized(s => genTree(5))) // TODO: hack

  implicit class Implies(self: Boolean) {
    def implies(other: => Boolean) = if(!self) true else other
  }
}